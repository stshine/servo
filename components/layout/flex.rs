/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! Layout for elements with a CSS `display` property of `flex`.

#![deny(unsafe_code)]

use app_units::{Au, MAX_AU};
use block::{BlockFlow, ISizeConstraintSolution} ;
use context::LayoutContext;
use display_list_builder::{DisplayListBuildState, FlexFlowDisplayListBuilding};
use euclid::Point2D;
use floats::FloatKind;
use flow;
use flow::{Flow, FlowClass, ImmutableFlowUtils, OpaqueFlow};
use flow::{INLINE_POSITION_IS_STATIC, IS_ABSOLUTELY_POSITIONED};
use flow_ref::{self, FlowRef};
use fragment::{Fragment, FragmentBorderBoxIterator, Overflow};
use gfx::display_list::StackingContext;
use gfx_traits::StackingContextId;
use layout_debug;
use model::{IntrinsicISizes, MaybeAuto, MinMaxConstraint};
use script_layout_interface::restyle_damage::{REFLOW, REFLOW_OUT_OF_FLOW};
use model::{specified, specified_or_none};
use std::cmp::{max, min};
use std::ops::Range;
use std::sync::Arc;
use style::computed_values::{box_sizing, border_collapse};
use style::computed_values::{flex_direction, flex_wrap, justify_content, align_content, align_self};
use style::logical_geometry::LogicalSize;
use style::properties::{ComputedValues, ServoComputedValues};
use style::servo::SharedStyleContext;
use style::values::computed::{LengthOrPercentage, LengthOrPercentageOrAuto};
use style::values::computed::{LengthOrPercentageOrAutoOrContent, LengthOrPercentageOrNone};

/// The size of an axis. May be a specified size, a min/max
/// constraint, or an unlimited size
#[derive(Debug)]
enum AxisSize {
    Definite(Au),
    MinMax(MinMaxConstraint),
    Infinite,
}

impl AxisSize {
    /// Generate a new available cross or main axis size from the specified size of the container,
    /// containing block size, min constraint, and max constraint
    pub fn new(size: LengthOrPercentageOrAuto, content_size: Option<Au>, min: LengthOrPercentage,
               max: LengthOrPercentageOrNone) -> AxisSize {
        match size {
            LengthOrPercentageOrAuto::Length(length) => AxisSize::Definite(length),
            LengthOrPercentageOrAuto::Percentage(percent) => {
                match content_size {
                    Some(size) => AxisSize::Definite(size.scale_by(percent)),
                    None => AxisSize::Infinite
                }
            },
            LengthOrPercentageOrAuto::Calc(calc) => {
                match content_size {
                    Some(size) => AxisSize::Definite(size.scale_by(calc.percentage())),
                    None => AxisSize::Infinite
                }
            },
            LengthOrPercentageOrAuto::Auto => {
                AxisSize::MinMax(MinMaxConstraint::new(content_size, min, max))
            }
        }
    }
}

// A mode describes which logical axis a flex axis is parallel with.
// The logical axises are inline and block, the flex axises are main and cross.
// When the flex container has flex-direction: column or flex-direction: column-reverse, the main axis
// should be block. Otherwise, it should be inline.
#[derive(Debug, Clone, Copy)]
enum Mode {
    Inline,
    Block
}

fn from_flex_basis(flex_basis: LengthOrPercentageOrAutoOrContent,
                   content_size: LengthOrPercentageOrAuto,
                   containing_length: Option<Au>) -> MaybeAuto {
    match (flex_basis, containing_length) {
        (LengthOrPercentageOrAutoOrContent::Length(length), _) =>
            MaybeAuto::Specified(length),
        (LengthOrPercentageOrAutoOrContent::Percentage(percent), Some(size)) =>
            MaybeAuto::Specified(size.scale_by(percent)),
        (LengthOrPercentageOrAutoOrContent::Percentage(_), None) => MaybeAuto::Auto,
        (LengthOrPercentageOrAutoOrContent::Calc(calc), Some(size)) =>
            MaybeAuto::Specified(calc.length() + size.scale_by(calc.percentage())),
        (LengthOrPercentageOrAutoOrContent::Calc(_), None) => MaybeAuto::Auto,
        (LengthOrPercentageOrAutoOrContent::Content, _) => MaybeAuto::Auto,
        (LengthOrPercentageOrAutoOrContent::Auto, Some(size)) => MaybeAuto::from_style(content_size, size),
        (LengthOrPercentageOrAutoOrContent::Auto, None) =>
            if let LengthOrPercentageOrAuto::Length(l) = content_size {
                MaybeAuto::Specified(l)
            } else {
                MaybeAuto::Auto
            }
    }
}

#[derive(Debug)]
struct FlexItem {
    pub main_size: Au,
    pub base_size: Au,
    pub min_size: Au,
    pub max_size: Au,
    pub flow: FlowRef,
    pub flex_grow: f32,
    pub flex_shrink: f32,
    pub order: i32,
    pub is_freezed: bool,
    //  this flow is a strut if it has property visibility::collapse.
    pub is_strut: bool
}

impl FlexItem {
    pub fn new(flow: FlowRef) -> FlexItem {
        let flex_grow;
        let flex_shrink;
        let order;
        {
            let style = flow.as_block().fragment.style();
            flex_grow = style.get_position().flex_grow;
            flex_shrink = style.get_position().flex_shrink;
            order = style.get_position().order;
            // TODO(stshine): for item with visibility:collapse, set is_strut to true.
        }

        FlexItem {
            main_size: Au(0),
            base_size: Au(0),
            min_size: Au(0),
            max_size: MAX_AU,
            flow: flow,
            flex_grow: flex_grow,
            flex_shrink: flex_shrink,
            order: order,
            is_freezed: false,
            is_strut: false
        }
    }

    pub fn init_sizes(&mut self, containing_length: Au, mode: Mode) {
        let block = self.flow.as_block();
        let style = block.fragment.style();
        match mode {
            // TODO(stshine): the definition of min-{width, height} in style component
            // should change to LengthOrPercentageOrAuto for automatic implied minimal size.
            // https://drafts.csswg.org/css-flexbox-1/#min-size-auto
            Mode::Inline => {
                let basis = from_flex_basis(style.get_position().flex_basis,
                                            style.content_inline_size(), Some(containing_length));

                let adjust_size = match style.get_position().box_sizing {
                    box_sizing::T::border_box => {
                        let margin = style.logical_margin();
                        (MaybeAuto::from_style(margin.inline_start, Au(0)).specified_or_zero() +
                         MaybeAuto::from_style(margin.inline_end, Au(0)).specified_or_zero())
                    }
                    box_sizing::T::content_box => self.flow.as_block().fragment.surrounding_intrinsic_inline_size(),
                };
                let content_size = block.base.intrinsic_inline_sizes.preferred_inline_size - adjust_size;
                self.base_size = basis.specified_or_default(content_size);
                self.max_size = specified_or_none(style.max_inline_size(), containing_length).unwrap_or(MAX_AU);
                self.min_size = specified(style.min_inline_size(), containing_length);
            },
            Mode::Block => {
                let basis = from_flex_basis(style.get_position().flex_basis,
                                            style.content_block_size(), Some(containing_length));
                // This method should be called in assign_block_size() pass that the block is already layouted.
                let content_size = match style.get_position().box_sizing {
                    box_sizing::T::border_box => block.fragment.border_box.size.block,
                    box_sizing::T::content_box => block.fragment.border_box.size.block -
                        block.fragment.border_padding.block_start_end(),
                };
                self.base_size = basis.specified_or_default(content_size);
                self.max_size = specified_or_none(style.max_block_size(), containing_length).unwrap_or(MAX_AU);
                self.min_size = specified(style.min_block_size(), containing_length);
            }
        }
    }

    pub fn outer_main_size(&mut self, containing_length: Au, mode: Mode) -> Au {
        let ref mut fragment = flow_ref::deref_mut(&mut self.flow).as_mut_block().fragment;
        let adjustment;
        match mode {
            Mode::Inline => {
                fragment.compute_border_and_padding(containing_length, border_collapse::T::separate);
                fragment.compute_inline_direction_margins(containing_length);
                fragment.compute_block_direction_margins(containing_length);
                adjustment = match fragment.style().get_position().box_sizing {
                    box_sizing::T::content_box =>
                        fragment.border_padding.inline_start_end() + fragment.margin.inline_start_end(),
                    box_sizing::T::border_box => fragment.margin.inline_start_end()
                };
            },
            Mode::Block => {
                adjustment = match fragment.style().get_position().box_sizing {
                    box_sizing::T::content_box =>
                        fragment.border_padding.block_start_end() + fragment.margin.block_start_end(),
                    box_sizing::T::border_box => fragment.margin.block_start_end()
                };
            }
        }
        max(self.min_size, min(self.base_size, self.max_size)) + adjustment
    }

    pub fn auto_margin_num(&self, mode: Mode) -> i32 {
        // FIXME(stshine): even a simple helper method like this involves a vtable lookup.
        // How to make it better?
        let margin = self.flow.as_block().fragment.style().logical_margin();
        let mut margin_num = 0;
        match mode {
            Mode::Inline => {
                if margin.inline_start == LengthOrPercentageOrAuto::Auto {
                    margin_num += 1;
                }
                if margin.inline_end == LengthOrPercentageOrAuto::Auto {
                    margin_num += 1;
                }
            }
            Mode::Block => {
                if margin.block_start == LengthOrPercentageOrAuto::Auto {
                    margin_num += 1;
                }
                if margin.block_end == LengthOrPercentageOrAuto::Auto {
                    margin_num += 1;
                }
            }
        }
        margin_num
    }
}

// TODO(stshine): More fields may be required to handle collapsed items and baseline alignment.
#[derive(Debug)]
struct FlexLine {
    pub range: Range<usize>,
    pub free_space: Au,
    pub auto_margin_count: i32,
    pub cross_size: Au,
}

impl FlexLine {
    pub fn new(range: Range<usize>, free_space: Au, auto_margin_count: i32) -> FlexLine {
        FlexLine {
            range: range,
            auto_margin_count: auto_margin_count,
            free_space: free_space,
            cross_size: Au(0)
        }
    }

    pub fn flex_resolve(&mut self, items: &mut [FlexItem], collapse: bool) {
        let mut total_grow = 0.0;
        let mut total_shrink = 0.0;
        let mut total_scaled = 0.0;
        let mut active_num = 0;
        for item in items.iter_mut().filter(|i| !(i.is_strut && collapse)) {
            item.main_size = max(item.min_size, min(item.base_size, item.max_size));
            if item.main_size != item.base_size
                || (self.free_space > Au(0) && item.flex_grow == 0.0)
                || (self.free_space < Au(0) && item.flex_shrink == 0.0) {
                    item.is_freezed = true;
                } else {
                    item.is_freezed = false;
                    total_grow += item.flex_grow;
                    total_shrink += item.flex_shrink;
                    total_scaled += item.flex_shrink * item.base_size.0 as f32;
                    active_num += 1;
                }
        }

        let initial_free_space = self.free_space;
        while self.free_space != Au(0) && active_num > 0 {
            self.free_space =
                if self.free_space > Au(0) {
                    min(initial_free_space.scale_by(total_grow), self.free_space)
                } else {
                    max(initial_free_space.scale_by(total_shrink), self.free_space)
                };

            let free_space = self.free_space;
            for item in items.iter_mut().filter(|i| !i.is_freezed).filter(|i| !(i.is_strut && collapse)) {
                let (factor, end_size) = if self.free_space > Au(0) {
                    (item.flex_grow / total_grow, item.max_size)
                } else {
                    (item.flex_shrink * item.base_size.0 as f32 / total_scaled, item.min_size)
                };
                let mut variation = free_space.scale_by(factor);
                if variation.0.abs() > (end_size - item.main_size).0.abs() {
                    variation = end_size - item.main_size;
                    item.main_size = end_size;
                    item.is_freezed = true;
                    active_num -= 1;
                    total_shrink -= item.flex_shrink;
                    total_grow -= item.flex_grow;
                    total_scaled -= item.flex_shrink * item.base_size.0 as f32;
                } else {
                    item.main_size += variation;
                }
                self.free_space -= variation;
            }
        }
    }
}

/// A block with the CSS `display` property equal to `flex`.
#[derive(Debug)]
pub struct FlexFlow {
    /// Data common to all block flows.
    block_flow: BlockFlow,
    /// The logical axis which the main axis will be parallel with.
    /// The cross axis will be parallel with the opposite logical axis.
    main_mode: Mode,
    /// The available main axis size
    available_main_size: AxisSize,
    /// The available cross axis size
    available_cross_size: AxisSize,
    /// List of flex lines in the container.
    lines: Vec<FlexLine>,
    /// List of flex-items that belong to this flex-container
    items: Vec<FlexItem>,
    /// True if the flex-direction is *-reversed
    main_reverse: bool,
    /// True if this flex container can be multiline.
    is_wrappable: bool,
    /// True if the cross direction is reversed.
    cross_reverse: bool
}

impl FlexFlow {
    pub fn from_fragment(fragment: Fragment,
                         flotation: Option<FloatKind>)
                         -> FlexFlow {
        let main_mode;
        let main_reverse;
        let is_wrappable;
        let cross_reverse;
        {
            let style = fragment.style();
            let (mode, reverse) = match style.get_position().flex_direction {
                flex_direction::T::row            => (Mode::Inline, false),
                flex_direction::T::row_reverse    => (Mode::Inline, true),
                flex_direction::T::column         => (Mode::Block, false),
                flex_direction::T::column_reverse => (Mode::Block, true),
            };
            main_mode = mode;
            main_reverse =
                reverse == style.writing_mode.is_bidi_ltr();
            let (wrappable, reverse) = match fragment.style.get_position().flex_wrap {
                flex_wrap::T::nowrap              => (false, false),
                flex_wrap::T::wrap                => (true, false),
                flex_wrap::T::wrap_reverse        => (true, true),
            };
            is_wrappable = wrappable;
            cross_reverse =
                reverse == (style.writing_mode.is_vertical() && style.writing_mode.is_vertical_lr());
        }
        FlexFlow {
            block_flow: BlockFlow::from_fragment(fragment, flotation),
            main_mode: main_mode,
            available_main_size: AxisSize::Infinite,
            available_cross_size: AxisSize::Infinite,
            lines: Vec::new(),
            items: Vec::new(),
            main_reverse: main_reverse,
            is_wrappable: is_wrappable,
            cross_reverse: cross_reverse
        }
    }

    fn get_flex_line(&mut self, container_size: Au) -> Option<FlexLine> {
        let start = if self.lines.len() == 0 {
            0
        } else {
            self.lines[self.lines.len()-1].range.end
        };
        if start == self.items.len() {
            return None;
        }
        let mut end = start;
        let mut total_line_size = Au(0);
        let mut margin_count = 0;

        let items = &mut self.items[start..];
        for mut item in items.iter_mut() {
            item.init_sizes(container_size, self.main_mode);
            let outer_main_size = item.outer_main_size(container_size, self.main_mode);
            if total_line_size + outer_main_size > container_size && end != start  && self.is_wrappable {
                break;
            }
            margin_count += item.auto_margin_num(self.main_mode);
            total_line_size += outer_main_size;
            end += 1;
        }

        let line = FlexLine::new(start..end, container_size - total_line_size, margin_count);
        Some(line)
    }

    // TODO(zentner): This function should use flex-basis.
    // Currently, this is the core of BlockFlow::bubble_inline_sizes() with all float logic
    // stripped out, and max replaced with union_nonbreaking_inline.
    fn inline_mode_bubble_inline_sizes(&mut self) {
        let fixed_width = match self.block_flow.fragment.style().get_position().width {
            LengthOrPercentageOrAuto::Length(_) => true,
            _ => false,
        };

        let mut computation = self.block_flow.fragment.compute_intrinsic_inline_sizes();
        if !fixed_width {
            for kid in &mut self.items {
                let base = flow::mut_base(flow_ref::deref_mut(&mut kid.flow));
                let is_absolutely_positioned = base.flags.contains(IS_ABSOLUTELY_POSITIONED);
                if !is_absolutely_positioned {
                    let flex_item_inline_sizes = IntrinsicISizes {
                        minimum_inline_size: base.intrinsic_inline_sizes.minimum_inline_size,
                        preferred_inline_size: base.intrinsic_inline_sizes.preferred_inline_size,
                    };
                    computation.union_nonbreaking_inline(&flex_item_inline_sizes);
                }
            }
        }
        self.block_flow.base.intrinsic_inline_sizes = computation.finish();
    }

    // TODO(zentner): This function should use flex-basis.
    // Currently, this is the core of BlockFlow::bubble_inline_sizes() with all float logic
    // stripped out.
    fn block_mode_bubble_inline_sizes(&mut self) {
        let fixed_width = match self.block_flow.fragment.style().get_position().width {
            LengthOrPercentageOrAuto::Length(_) => true,
            _ => false,
        };

        let mut computation = self.block_flow.fragment.compute_intrinsic_inline_sizes();
        if !fixed_width {
            for kid in &mut self.items {
                let base = flow::mut_base(flow_ref::deref_mut(&mut kid.flow));
                let is_absolutely_positioned = base.flags.contains(IS_ABSOLUTELY_POSITIONED);
                if !is_absolutely_positioned {
                    computation.content_intrinsic_sizes.minimum_inline_size =
                        max(computation.content_intrinsic_sizes.minimum_inline_size,
                            base.intrinsic_inline_sizes.minimum_inline_size);

                    computation.content_intrinsic_sizes.preferred_inline_size =
                        max(computation.content_intrinsic_sizes.preferred_inline_size,
                            base.intrinsic_inline_sizes.preferred_inline_size);
                }
            }
        }
        self.block_flow.base.intrinsic_inline_sizes = computation.finish();
    }

    // TODO(zentner): This function needs to be radically different for multi-line flexbox.
    // Currently, this is the core of BlockFlow::propagate_assigned_inline_size_to_children() with
    // all float and table logic stripped out.
    fn block_mode_assign_inline_sizes(&mut self,
                                      _shared_context: &SharedStyleContext,
                                      inline_start_content_edge: Au,
                                      inline_end_content_edge: Au,
                                      content_inline_size: Au) {
        let _scope = layout_debug_scope!("flex::block_mode_assign_inline_sizes");
        debug!("block_mode_assign_inline_sizes");

        // FIXME (mbrubeck): Get correct mode for absolute containing block
        let containing_block_mode = self.block_flow.base.writing_mode;

        let container_block_size = match self.available_main_size {
            AxisSize::Definite(length) => Some(length),
            _ => None
        };
        let container_inline_size = match self.available_cross_size {
            AxisSize::Definite(length) => length,
            AxisSize::MinMax(ref constraint) => constraint.clamp(content_inline_size),
            AxisSize::Infinite => content_inline_size
        };
        for kid in &mut self.items {
            {
                let kid_base = flow::mut_base(flow_ref::deref_mut(&mut kid.flow));
                kid_base.block_container_explicit_block_size = container_block_size;
                if kid_base.flags.contains(INLINE_POSITION_IS_STATIC) {
                    // The inline-start margin edge of the child flow is at our inline-start content edge,
                    // and its inline-size is our content inline-size.
                    kid_base.position.start.i =
                        if kid_base.writing_mode.is_bidi_ltr() == containing_block_mode.is_bidi_ltr() {
                            inline_start_content_edge
                        } else {
                            // The kid's inline 'start' is at the parent's 'end'
                            inline_end_content_edge
                        };
                }
                kid_base.block_container_inline_size = container_inline_size;
                kid_base.block_container_writing_mode = containing_block_mode;
                kid_base.position.start.i = inline_start_content_edge;
            }
        }
    }

    fn inline_mode_assign_inline_sizes(&mut self,
                                       _shared_context: &SharedStyleContext,
                                       inline_start_content_edge: Au,
                                       _inline_end_content_edge: Au,
                                       content_inline_size: Au) {
        let _scope = layout_debug_scope!("flex::inline_mode_assign_inline_sizes");
        debug!("inline_mode_assign_inline_sizes");

        debug!("content_inline_size = {:?}", content_inline_size);

        let child_count = ImmutableFlowUtils::child_count(self as &Flow) as i32;
        debug!("child_count = {:?}", child_count);
        if child_count == 0 {
            return;
        }

        let inline_size = match self.available_main_size {
            AxisSize::Definite(length) => length,
            AxisSize::MinMax(ref constraint) => constraint.clamp(content_inline_size),
            AxisSize::Infinite => content_inline_size,
        };

        let container_mode = self.block_flow.base.block_container_writing_mode;
        self.block_flow.base.position.size.inline = inline_size;

        while let Some(mut line) = self.get_flex_line(inline_size) {
            let mut items = &mut self.items[line.range.clone()];
            line.flex_resolve(&mut items, false);
            //TODO(stshine): if this flex line contain children that have
            //property visibility:hidden, exclude them and resolve again.

            let item_count = items.len() as i32;
            let free_main = max(line.free_space, Au(0));
            let mut cur_i = inline_start_content_edge;
            match self.block_flow.fragment.style().get_position().justify_content {
                justify_content::T::center => {
                    cur_i += free_main / 2 ;
                }
                justify_content::T::space_around => {
                    cur_i += free_main /(item_count*2);
                }
                justify_content::T::flex_end => {
                    cur_i += free_main;
                }
                _ => {}
            }

            for item in items.iter_mut() {
                let mut block = flow_ref::deref_mut(&mut item.flow).as_mut_block();
                // TODO(stshine): should this be done during construction?
                block.mark_as_flex();
                let margin = block.fragment.style().logical_margin();
                let auto_len =
                    if line.auto_margin_count == 0 {
                        Au(0)
                    } else {
                        free_main / line.auto_margin_count
                    };
                let margin_inline_start = MaybeAuto::from_style(margin.inline_start, inline_size)
                    .specified_or_default(auto_len);
                let margin_inline_end = MaybeAuto::from_style(margin.inline_end, inline_size)
                    .specified_or_default(auto_len);
                let item_inline_size = item.main_size -
                    match block.fragment.style().get_position().box_sizing {
                        box_sizing::T::border_box => block.fragment.border_padding.inline_start_end(),
                        box_sizing::T::content_box => Au(0),
                    };
                set_inline_size_constraint_solutions(block,
                                                     ISizeConstraintSolution {
                                                         inline_start: Au(0),
                                                         inline_size: item_inline_size,
                                                         margin_inline_start: margin_inline_start,
                                                         margin_inline_end: margin_inline_end
                                                     });
                block.base.block_container_writing_mode = container_mode;
                let item_outer_size = item_inline_size + block.fragment.border_padding.inline_start_end()
                    + block.fragment.margin.inline_start_end();
                if !self.main_reverse {
                    block.base.position.start.i = cur_i;
                } else {
                    block.base.position.start.i =
                        inline_start_content_edge + content_inline_size
                        + inline_start_content_edge - cur_i  - item_outer_size;
                };
                cur_i += item_outer_size +
                    if free_main != Au(0) && line.auto_margin_count == 0 {
                        match self.block_flow.fragment.style().get_position().justify_content {
                            justify_content::T::space_between => {
                                if item_count == 1 {
                                    Au(0)
                                } else {
                                    free_main / (item_count-1)
                                }
                            },
                            justify_content::T::space_around => {
                                free_main / item_count
                            },
                            _ => {Au(0)},
                        }
                    } else {
                        Au(0)
                    };
            }
            self.lines.push(line);
        }
    }

    // TODO(zentner): This function should actually flex elements!
    fn block_mode_assign_block_size<'a>(&mut self, layout_context: &'a LayoutContext<'a>) {
        let mut cur_b = if !self.main_reverse {
            self.block_flow.fragment.border_padding.block_start
        } else {
            self.block_flow.fragment.border_box.size.block
        };
        for kid in &mut self.items {
            let base = flow::mut_base(flow_ref::deref_mut(&mut kid.flow));
            if !self.main_reverse {
                base.position.start.b = cur_b;
                cur_b = cur_b + base.position.size.block;
            } else {
                cur_b = cur_b - base.position.size.block;
                base.position.start.b = cur_b;
            }
        }
        self.block_flow.assign_block_size(layout_context)
    }

    fn inline_mode_assign_block_size<'a>(&mut self, layout_context: &'a LayoutContext<'a>) {
        let _scope = layout_debug_scope!("flex::inline_mode_assign_block_size");

        let padding_and_borders = self.block_flow.fragment.border_padding.inline_start_end();
        let content_inline_size = self.block_flow.fragment.border_box.size.inline - padding_and_borders;
        let inline_size = match self.available_main_size {
            AxisSize::Definite(length) => length,
            AxisSize::MinMax(ref constraint) => constraint.clamp(content_inline_size),
            AxisSize::Infinite => content_inline_size,
        };

        let mut cur_b = self.block_flow.fragment.border_padding.block_start;

        let line_count = self.lines.len() as i32;
        let line_align = self.block_flow.fragment.style().get_position().align_content;
        let mut total_cross_size = Au(0);
        let mut line_interval = Au(0);

        for line in self.lines.iter_mut() {
            for item in &self.items[line.range.clone()] {
                let ref fragment = item.flow.as_block().fragment;
                line.cross_size = max(line.cross_size,
                                      fragment.border_box.size.block + fragment.margin.block_start_end());
            }
            total_cross_size += line.cross_size;
        }

        let parent_container_size = self.block_flow.explicit_block_containing_size(layout_context);
        if let Some(free_space) = self.block_flow.explicit_block_size(parent_container_size)
            .and_then(|i| if i - total_cross_size > Au(0) {Some(i - total_cross_size)} else { None }) {
                match line_align {
                    align_content::T::center=> {
                        cur_b += free_space / 2 ;
                    }
                    align_content::T::space_around => {
                        cur_b += free_space /(line_count*2);
                    }
                    align_content::T::flex_end => {
                        cur_b += free_space;
                    }
                    _ => {}
                }

                if line_align == align_content::T::stretch {
                    for line in self.lines.iter_mut() {
                        line.cross_size += free_space / line_count;
                    }
                } else {
                    line_interval = match line_align {
                        align_content::T::space_between => {
                            if line_count == 1 {
                                Au(0)
                            } else {
                                free_space/(line_count-1)
                            }
                        },
                        align_content::T::space_around => {
                            free_space/line_count
                        },
                        _ => { Au(0) },
                    };
                }
                total_cross_size += free_space;
            }

        for line in &self.lines {
            for mut item in self.items[line.range.clone()].iter_mut() {
                let mut block = flow_ref::deref_mut(&mut item.flow).as_mut_block();
                let margin = block.fragment.style().logical_margin();
                let block_size = block.base.position.size.block;
                let free_cross = line.cross_size - block_size;
                let mut auto_margin_count = 0;
                let mut auto_len = Au(0);
                if margin.block_start == LengthOrPercentageOrAuto::Auto {
                    auto_len = free_cross;
                    auto_margin_count += 1;
                }
                if margin.block_end == LengthOrPercentageOrAuto::Auto {
                    auto_len = free_cross / 2;
                    auto_margin_count += 1;
                }

                let margin_block_start = MaybeAuto::from_style(margin.block_start, inline_size)
                    .specified_or_default(auto_len);
                let margin_block_end = MaybeAuto::from_style(margin.block_end, inline_size)
                    .specified_or_default(auto_len);
                let free_cross = line.cross_size - block_size - margin_block_start - margin_block_end;
                if auto_margin_count == 0 {
                    let self_align = block.fragment.style().get_position().align_self;
                    // TODO(stshine): support baseline alignment.
                    let flex_cross = match self_align {
                        align_self::T::flex_end => free_cross,
                        align_self::T::center => free_cross / 2,
                        _ => Au(0),
                    };
                    block.base.position.start.b = cur_b + margin_block_start + flex_cross;
                    if self_align == align_self::T::stretch
                        && block.fragment.style().content_block_size() == LengthOrPercentageOrAuto::Auto {
                            block.base.block_container_explicit_block_size = Some(line.cross_size);
                            block.base.position.size.block =
                                line.cross_size - margin_block_start - margin_block_end;
                            block.fragment.border_box.size.block =
                                line.cross_size - margin_block_start - margin_block_end;
                            // FIXME(stshine): item with `align-self: stretch` and auto cross size should
                            // act as if it has a fixed size, all child blocks should resolve against it.
                            // block.assign_block_size(layout_context);
                        }
                } else {
                    block.base.position.start.b = cur_b + margin_block_start;
                }
            }
            cur_b += line_interval + line.cross_size;
        }
        let total_block_size = total_cross_size + self.block_flow.fragment.border_padding.block_start_end();
        self.block_flow.fragment.border_box.size.block = total_block_size;
        self.block_flow.base.position.size.block = total_block_size;
    }
}

impl Flow for FlexFlow {
    fn class(&self) -> FlowClass {
        FlowClass::Flex
    }

    fn as_block(&self) -> &BlockFlow {
        &self.block_flow
    }

    fn as_mut_block(&mut self) -> &mut BlockFlow {
        &mut self.block_flow
    }

    fn mark_as_root(&mut self) {
        self.block_flow.mark_as_root();
    }

    fn bubble_inline_sizes(&mut self) {
        let _scope = layout_debug_scope!("flex::bubble_inline_sizes {:x}",
                                         self.block_flow.base.debug_id());

        // Flexbox Section 9.0: Generate anonymous flex items:
        // This part was handled in the flow constructor.

        // Flexbox Section 9.1: Re-order the flex items according to their order.

        let mut items = self.block_flow.base.children.iter_flow_ref_mut()
            .filter(|flow| !flow.as_block().base.flags.contains(IS_ABSOLUTELY_POSITIONED))
            .map(|flow| FlexItem::new(flow.clone()))
            .collect::<Vec<FlexItem>>();

        items.sort_by_key(|item| item.order);
        self.items = items;

        match self.main_mode {
            Mode::Inline => self.inline_mode_bubble_inline_sizes(),
            Mode::Block  => self.block_mode_bubble_inline_sizes()
        }
    }

    fn assign_inline_sizes(&mut self, shared_context: &SharedStyleContext) {
        let _scope = layout_debug_scope!("flex::assign_inline_sizes {:x}", self.block_flow.base.debug_id());
        debug!("assign_inline_sizes");

        if !self.block_flow.base.restyle_damage.intersects(REFLOW_OUT_OF_FLOW | REFLOW) {
            return
        }

        // Our inline-size was set to the inline-size of the containing block by the flow's parent.
        // Now compute the real value.
        let containing_block_inline_size = self.block_flow.base.block_container_inline_size;
        self.block_flow.compute_used_inline_size(shared_context, containing_block_inline_size);
        if self.block_flow.base.flags.is_float() {
            self.block_flow.float.as_mut().unwrap().containing_inline_size = containing_block_inline_size
        }

        let (available_block_size, available_inline_size) = {
            let style = &self.block_flow.fragment.style;
            let (specified_block_size, specified_inline_size) = if style.writing_mode.is_vertical() {
                (style.get_position().width, style.get_position().height)
            } else {
                (style.get_position().height, style.get_position().width)
            };

            let available_inline_size = AxisSize::new(specified_inline_size,
                                                      Some(self.block_flow.base.block_container_inline_size),
                                                      style.min_inline_size(),
                                                      style.max_inline_size());

            let available_block_size = AxisSize::new(specified_block_size,
                                                     self.block_flow.base.block_container_explicit_block_size,
                                                     style.min_block_size(),
                                                     style.max_block_size());
            (available_block_size, available_inline_size)
        };

        // Move in from the inline-start border edge.
        let inline_start_content_edge = self.block_flow.fragment.border_box.start.i +
            self.block_flow.fragment.border_padding.inline_start;

        debug!("inline_start_content_edge = {:?}", inline_start_content_edge);

        let padding_and_borders = self.block_flow.fragment.border_padding.inline_start_end();

        // Distance from the inline-end margin edge to the inline-end content edge.
        let inline_end_content_edge =
            self.block_flow.fragment.margin.inline_end +
            self.block_flow.fragment.border_padding.inline_end;

        debug!("padding_and_borders = {:?}", padding_and_borders);
        debug!("self.block_flow.fragment.border_box.size.inline = {:?}",
               self.block_flow.fragment.border_box.size.inline);
        let content_inline_size = self.block_flow.fragment.border_box.size.inline - padding_and_borders;

        match self.main_mode {
            Mode::Inline => {
                self.available_main_size = available_inline_size;
                self.available_cross_size = available_block_size;
                self.inline_mode_assign_inline_sizes(shared_context,
                                                     inline_start_content_edge,
                                                     inline_end_content_edge,
                                                     content_inline_size)
            },
            Mode::Block  => {
                self.available_main_size = available_block_size;
                self.available_cross_size = available_inline_size;
                self.block_mode_assign_inline_sizes(shared_context,
                                                    inline_start_content_edge,
                                                    inline_end_content_edge,
                                                    content_inline_size)
            }
        }
    }

    fn assign_block_size<'a>(&mut self, layout_context: &'a LayoutContext<'a>) {
        self.block_flow.assign_block_size(layout_context);
        match self.main_mode {
            Mode::Inline =>
                self.inline_mode_assign_block_size(layout_context),
            Mode::Block  =>
                self.block_mode_assign_block_size(layout_context)
        }
    }

    fn compute_absolute_position(&mut self, layout_context: &LayoutContext) {
        self.block_flow.compute_absolute_position(layout_context)
    }

    fn place_float_if_applicable<'a>(&mut self) {
        self.block_flow.place_float_if_applicable()
    }

    fn update_late_computed_inline_position_if_necessary(&mut self, inline_position: Au) {
        self.block_flow.update_late_computed_inline_position_if_necessary(inline_position)
    }

    fn update_late_computed_block_position_if_necessary(&mut self, block_position: Au) {
        self.block_flow.update_late_computed_block_position_if_necessary(block_position)
    }

    fn build_display_list(&mut self, state: &mut DisplayListBuildState) {
        self.build_display_list_for_flex(state);
    }

    fn collect_stacking_contexts(&mut self,
                                 parent_id: StackingContextId,
                                 contexts: &mut Vec<Box<StackingContext>>)
                                 -> StackingContextId {
        self.block_flow.collect_stacking_contexts(parent_id, contexts)
    }

    fn repair_style(&mut self, new_style: &Arc<ServoComputedValues>) {
        self.block_flow.repair_style(new_style)
    }

    fn compute_overflow(&self) -> Overflow {
        self.block_flow.compute_overflow()
    }

    fn generated_containing_block_size(&self, flow: OpaqueFlow) -> LogicalSize<Au> {
        self.block_flow.generated_containing_block_size(flow)
    }

    fn iterate_through_fragment_border_boxes(&self,
                                             iterator: &mut FragmentBorderBoxIterator,
                                             level: i32,
                                             stacking_context_position: &Point2D<Au>) {
        self.block_flow.iterate_through_fragment_border_boxes(iterator, level, stacking_context_position);
    }

    fn mutate_fragments(&mut self, mutator: &mut FnMut(&mut Fragment)) {
        self.block_flow.mutate_fragments(mutator);
    }
}

// This function is copied from block::ISizeAndMarginsComputer.
fn set_inline_size_constraint_solutions(block: &mut BlockFlow,
                                        solution: ISizeConstraintSolution) {
    let inline_size;
    let extra_inline_size_from_margin;
    {
        let block_mode = block.base.writing_mode;

        // FIXME (mbrubeck): Get correct containing block for positioned blocks?
        let container_mode = block.base.block_container_writing_mode;
        let container_size = block.base.block_container_inline_size;

        let fragment = block.fragment();
        fragment.margin.inline_start = solution.margin_inline_start;
        fragment.margin.inline_end = solution.margin_inline_end;

        // The associated fragment has the border box of this flow.
        inline_size = solution.inline_size + fragment.border_padding.inline_start_end();
        fragment.border_box.size.inline = inline_size;

        // Start border edge.
        // FIXME (mbrubeck): Handle vertical writing modes.
        fragment.border_box.start.i =
            if container_mode.is_bidi_ltr() == block_mode.is_bidi_ltr() {
                fragment.margin.inline_start
            } else {
                // The parent's "start" direction is the child's "end" direction.
                container_size - inline_size - fragment.margin.inline_end
            };

        // To calculate the total size of this block, we also need to account for any
        // additional size contribution from positive margins. Negative margins means the block
        // isn't made larger at all by the margin.
        extra_inline_size_from_margin = max(Au(0), fragment.margin.inline_start) +
            max(Au(0), fragment.margin.inline_end);
    }

    // We also resize the block itself, to ensure that overflow is not calculated
    // as the inline-size of our parent. We might be smaller and we might be larger if we
    // overflow.
    flow::mut_base(block).position.size.inline = inline_size + extra_inline_size_from_margin;
}

