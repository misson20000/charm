use crate::model::addr;
use crate::model::datapath;
use crate::model::listing::cursor;
use crate::view::helpers;
use crate::view::gsc;
use crate::view::listing;
use crate::view::listing::token_view::TokenView;
use crate::view::listing::facet::cursor::CursorView;

use gtk::graphene;

pub fn render(token_view: &mut TokenView, extent: addr::Extent, snapshot: &gtk::Snapshot, cursor: &CursorView, has_cursor: bool, render: &listing::RenderDetail, pos: &mut graphene::Point) {
    let hex_cursor = match &cursor.cursor.class {
        cursor::CursorClass::Hexdump(hxc) if has_cursor => Some(hxc),
        _ => None,
    };
    
    for i in 0..extent.length().bytes {
        let byte_record = token_view.data_cache.get(i as usize).copied().unwrap_or(datapath::ByteRecord::default());

        if i != 0 {
            /* insert a gutter between every byte */
            pos.set_x(pos.x() + helpers::pango_unscale(render.gsc_mono.space_width()));
            
            if i % 8 == 0 && i != 0 {
                /* insert an additional gutter every 8 bytes */
                pos.set_x(pos.x() + helpers::pango_unscale(render.gsc_mono.space_width()));
            }
        }
        
        for low_nybble in [false, true] {
            let nybble = if low_nybble { byte_record.value & 0xf } else { byte_record.value >> 4 };
            let has_cursor = hex_cursor.map_or(false, |hxc| hxc.offset.bytes == i && hxc.low_nybble == low_nybble);

            let digit = gsc::Entry::Digit(nybble);

            if !byte_record.pending && byte_record.loaded {
                if has_cursor {
                    /* the cursor is over this nybble */
                    render.gsc_mono.print_with_cursor(&snapshot, digit, &render.config, cursor, pos);
                } else {
                    /* the cursor is not over this nybble */
                    render.gsc_mono.print(&snapshot, digit, &render.config.text_color, pos);
                }
            } else {
                /* Draw a placeholder, instead. */
                if let Some(gs) = render.gsc_mono.get(digit) {
                    let (_ink, logical) = gs.clone().extents(&render.font_mono);
                    
                    snapshot.append_color(&render.config.placeholder_color, &graphene::Rect::new(
                        pos.x() + helpers::pango_unscale(logical.x()),
                        pos.y() + helpers::pango_unscale(logical.y()),
                        helpers::pango_unscale(logical.width()),
                        helpers::pango_unscale(logical.height())));

                    if has_cursor {
                        snapshot.append_color(&render.config.cursor_bg_color, &graphene::Rect::new(
                            pos.x() + helpers::pango_unscale(logical.x()) + cursor.get_bonk(),
                            pos.y() + helpers::pango_unscale(logical.y()),
                            helpers::pango_unscale(logical.width()),
                            helpers::pango_unscale(logical.height())));                                    
                    }

                    pos.set_x(pos.x() + helpers::pango_unscale(logical.width()));
                }
            }
        }
    }
}
