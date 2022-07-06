//--> Type Aliases <--

pub type ASCIIString = Vec<u8>;

//--> Functions <--

fn ascii_to_byte(c: char) -> Option<u8> {
	if c.is_ascii() {
		let mut buf: [u8; 1] = [0x00];
		c.encode_utf8(&mut buf);
		Some(buf[0])
	} else { None }
}

pub fn byte_to_ascii(b: u8) -> Option<char> { char::from_u32(b as u32) }

pub fn make_ascii_string(s: &str) -> Option<ASCIIString> {
	let mut char_stack = s.chars().rev().collect::<Vec<char>>();

	let mut string: ASCIIString = Vec::new();

	loop {
		let char0 = char_stack.pop()?;

		if char0 == '\\' {
			// Time to handle escape sequences!
			match char_stack.pop()? {
				'0' => string.push(0x00),
				'a' => string.push(0x07),
				'b' => string.push(0x08),
				't' => string.push(0x09),
				'n' => string.push(0x0a),
				'v' => string.push(0x0b),
				'f' => string.push(0x0c),
				'r' => string.push(0x0d),
				'e' => string.push(0x1b),
				'"' => string.push(0x22),
				'\\' => string.push(0x5C),
				'x' => {
					let mut value_string = String::new();

					value_string.push(char_stack.pop()?);
					value_string.push(char_stack.pop()?);

					if let Ok(b) = u8::from_str_radix(&value_string, 16) {
						string.push(b);
					} else { return None }
				},
				_ => return None
			}
		} else { string.push(ascii_to_byte(char0)?); }

		if char_stack.is_empty() { break; }
	}

	Some(string)
}

pub fn make_raw_ascii_string(s: &str) -> Option<ASCIIString> {
	let characters = s.chars().collect::<Vec<char>>();
	let mut string: Vec<u8> = Vec::new();

	for character in characters {
		string.push(ascii_to_byte(character)?);
	}

	Some(string)
}

pub fn make_ascii_character(s: &str) -> Option<u8> {
	let chars = s.chars().collect::<Vec<char>>();

	if chars.is_empty() { return None }
	
	if chars[0] == '\\' {
		if chars.len() == 1 { return None }
		
		// Time to handle escape sequences!
		match chars[1] {
			'0' => if chars.len() == 2 { Some(0x00) } else { None },
			'a' => if chars.len() == 2 { Some(0x07) } else { None },
			'b' => if chars.len() == 2 { Some(0x08) } else { None },
			't' => if chars.len() == 2 { Some(0x09) } else { None },
			'n' => if chars.len() == 2 { Some(0x0a) } else { None },
			'v' => if chars.len() == 2 { Some(0x0b) } else { None },
			'f' => if chars.len() == 2 { Some(0x0c) } else { None },
			'r' => if chars.len() == 2 { Some(0x0d) } else { None },
			'e' => if chars.len() == 2 { Some(0x1b) } else { None },
			'\'' => if chars.len() == 2 { Some(0x27) } else { None },
			'\\' => if chars.len() == 2 { Some(0x5C) } else { None },
			'x' => if chars.len() == 4 {
				let mut value_string = String::new();

				value_string.push(chars[2]);
				value_string.push(chars[3]);

				u8::from_str_radix(&value_string, 16).ok()
			} else { None },
			_ => None
		}
	} else if chars.len() == 1 { ascii_to_byte(chars[0]) } else { None }
}