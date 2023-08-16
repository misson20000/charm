use crate::model::addr;
use crate::model::document::structure;
use crate::model::listing::cursor;
use crate::model::selection;
use crate::view::helpers;
use crate::view::gsc;
use crate::view::listing;
use crate::view::listing::token_view::TokenView;
use crate::view::listing::facet::cursor::CursorView;

use gtk::prelude::*;
use gtk::graphene;

pub fn render(
    token_view: &mut TokenView,
    length: addr::Size,
    snapshot: &gtk::Snapshot,
    cursor: &CursorView,
    has_cursor: bool,
    selection: selection::listing::TokenIntersection,
    render: &listing::RenderDetail,
    pos: &mut graphene::Point
) {
    let hex_cursor = match &cursor.cursor.class {
        cursor::CursorClass::Hexdump(hxc) if has_cursor => Some(hxc),
        _ => None,
    };

    let offset_in_line = match token_view.token.node.props.content_display {
        structure::ContentDisplay::Hexdump { line_pitch, .. } => {
            (token_view.token.offset % line_pitch).bytes as i64
        },
        _ => 0
    };

    let (_, logical_space) = render.gsc_mono.get(gsc::Entry::Space).unwrap().clone().extents(&render.font_mono);

    let extent = addr::Extent::sized(token_view.token.offset.to_addr(), length);
    
    for i in (-offset_in_line)..length.bytes as i64 {
        if (i + offset_in_line) != 0 {
            /* insert a gutter between every byte */
            let mut gutter_width = helpers::pango_unscale(logical_space.width());

            if (i + offset_in_line) % 8 == 0 && (i + offset_in_line) != 0 {
                /* insert an additional gutter every 8 bytes */
                gutter_width+= helpers::pango_unscale(logical_space.width());
            }
            
            if i >= 1 {
                let gutter_extent = addr::Extent::sized(((i as u64) - 1).into(), 2.into()).rebase(extent.begin).intersection(extent);
                let gutter_selected = gutter_extent.map_or(false, |ge| selection.includes(ge));

                if gutter_selected {
                    snapshot.append_color(&render.config.selection_color, &graphene::Rect::new(
                        pos.x() + helpers::pango_unscale(logical_space.x()),
                        pos.y() + helpers::pango_unscale(logical_space.y()),
                        gutter_width,
                        helpers::pango_unscale(logical_space.height()) - 1.0));
                }
            }
            
            pos.set_x(pos.x() + gutter_width);
        }

        /* if offset_in_line is set, skip rendering these slots. */
        if i < 0 {
            pos.set_x(pos.x() + helpers::pango_unscale(render.gsc_mono.space_width()));
            pos.set_x(pos.x() + helpers::pango_unscale(render.gsc_mono.space_width()));
            continue;
        }

        let byte_extent = addr::Extent::sized((i as u64).into(), addr::unit::BYTE).rebase(extent.begin).intersection(extent);
        let byte_record = token_view.data_cache.get(i as usize).copied().unwrap_or_default();

        let selected = byte_extent.map_or(false, |be| selection.includes(be));
        let pending = byte_record.pending || !byte_record.loaded;
        
        for low_nybble in [false, true] {
            let nybble = if low_nybble { byte_record.value & 0xf } else { byte_record.value >> 4 };
            let has_cursor = hex_cursor.map_or(false, |hxc| hxc.offset.bytes == i as u64 && hxc.low_nybble == low_nybble);
            
            let digit = if pending { gsc::Entry::Space } else { gsc::Entry::Digit(nybble) };

            render.gsc_mono.begin(digit, &render.config.text_color, pos)
                .selected(selected, &render.config.selection_color)
                .cursor(has_cursor, cursor, &render.config.cursor_fg_color, &render.config.cursor_bg_color)
                .placeholder(pending, &render.config.placeholder_color)
                .render(snapshot);
        }
    }
}

pub fn render_asciidump(
    token_view: &mut TokenView,
    length: addr::Size,
    snapshot: &gtk::Snapshot,
    _cursor: &CursorView,
    _has_cursor: bool,
    selection: selection::listing::TokenIntersection,
    render: &listing::RenderDetail,
    pos: &mut graphene::Point) {
    let offset_in_line = match token_view.token.node.props.content_display {
        structure::ContentDisplay::Hexdump { line_pitch, .. } => {
            (token_view.token.offset % line_pitch).bytes as i32
        },
        _ => 0
    };

    pos.set_x(pos.x() + helpers::pango_unscale(render.gsc_mono.space_width() * offset_in_line));

    let extent = addr::Extent::sized(token_view.token.offset.to_addr(), length);
    
    for i in 0..length.bytes {
        let byte_extent = addr::Extent::sized(i.into(), addr::unit::BYTE).rebase(extent.begin).intersection(extent);
        let byte_record = token_view.data_cache.get(i as usize).copied().unwrap_or_default();
        let selected = byte_extent.map_or(false, |be| selection.includes(be));
        let pending = byte_record.pending || !byte_record.loaded;
        
        let digit = if pending { gsc::Entry::Space } else { gsc::Entry::PrintableAscii(byte_record.value) };

        render.gsc_mono.begin(digit, &render.config.text_color, pos)
            .selected(selected, &render.config.selection_color)
            .placeholder(pending, &render.config.placeholder_color)
            .render(snapshot);
    }
}

pub fn pick_position(
    _token_view: &TokenView,
    _length: addr::Size,
    _x: f32) -> Option<(structure::Path, addr::Address, usize)> {
    todo!();
}
