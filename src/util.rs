pub fn nybble_to_hex(nyb: u8) -> u8 {
		if nyb < 10 {
				'0' as u8 + nyb
		} else {
				'a' as u8 + (nyb - 10)
		}
}
