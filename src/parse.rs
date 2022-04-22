//--> Imports <--

use super::lex::{Token, TokenStream, Const, Addr, Ptr, Wrd};
use crate::StringList;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

//--> Type Aliases <--

type FileAsmResult = Result<(FileAsm, StringList), (StringList, StringList)>;

type ProgAsmResult = Result<(ProgAsm, StringList), (StringList, StringList)>;

//--> Structs <--

// an assembly source code file
pub struct FileAsm {
	// symbols, indexed by identifier
	pub symbols: HashMap<String, Symbol>,
	// labels, global and local, indexed by line number
	pub labels: HashMap<usize, Label>,
	// instructions and directives, indexed by line number
	pub code: HashMap<usize, Code>
}

// a linked assembly program
pub struct ProgAsm {
	// symbols, indexed by identifier
	pub symbols: HashMap<String, Symbol>,
	// labels, global and local, indexed by absolute address
	pub labels: HashMap<u32, Label>,
	// instructions and directives, indexed by absolute address
	pub code: HashMap<u32, Code>
}

pub struct Label {
	pub id: String,
	pub local: bool
}

//--> Enums <--

// code
pub enum Code {
	Instruction(Inst),
	Directive(Dir)
}

// instructions
pub enum Inst {
	// nop
	NoOp,
	// mvr #/a/(a)/zb/(zb)/dp/p/ipr/spr r
	MoveToReg(MoveArg, Reg),
	// mvm r a/(a)/zb/(zb)/dp/p
	MoveToMem(Reg, MemArg),
	// mvrx a/zb/dp/p r
	MoveToRegX(MemArg, Reg),
	// mvry a/zb/dp/p r
	MoveToRegY(MemArg, Reg),
	// mvmx r a/zb/dp/p
	MoveToMemX(Reg, MemArg),
	// mvmy r a/zb/dp/p
	MoveToMemY(Reg, MemArg),
	// srz r
	SetRegZero(Reg),
	// smz a/(a)/zb/(zb)/dp/p
	SetMemZero(MemArg),
	// smzx a/zb/dp/p
	SetMemZeroX(MemArg),
	// smzy a/zb/dp/p
	SetMemZeroY(MemArg),
	// hacf
	HaltCatchFire,
	// trc r r
	TrxRegContents(Reg, Reg),
	// src r r
	SwpRegContents(Reg, Reg),
	// sspx
	StoreStackPtrX,
	// gspx
	GetStackPtrX,
	// ssb #
	SetStackBank(ImmArg),
	// sdp #
	SetDirectPage(ImmArg),
	// push r
	Push(Reg),
	// pop r
	Pop(Reg),
	// pshif
	PushIntFlags,
	// popif
	PopIntFlags,
	// and #/r r
	And(ALArg, Reg),
	// or #/r r
	Or(ALArg, Reg),
	// eor #/r r
	ExclOr(ALArg, Reg),
	// addi #/ir ir
	AddInt(ALArg, Reg),
	// adci #/ir ir
	AddCarryInt(ALArg, Reg),
	// subi #/ir ir
	SubInt(ALArg, Reg),
	// sbci #/ir ir
	SubCarryInt(ALArg, Reg),
	// not r
	Not(Reg),
	// shl r
	ShiftLeft(Reg),
	// shr r
	ShiftRight(Reg),
	// rol r
	RotateLeft(Reg),
	// ror r
	RotateRight(Reg),
	// pcnt r r
	PopCount(Reg, Reg),
	// vcnt r r
	VacCount(Reg, Reg),
	// swz # r
	Swizzle(ImmArg, Reg),
	// set # r
	Set(ImmArg, Reg),
	// clr # r
	Clear(ImmArg, Reg),
	// eirq
	EnableIRQs,
	// dirq
	DisableIRQs,
	// sed
	SetFlagDecimal,
	// cld
	ClearFlagDecimal,
	// ses
	SetFlagSign,
	// cls
	ClearFlagSign,
	// sen
	SetFlagNegative,
	// cln
	ClearFlagNegative,
	// seh
	SetFlagHalfCarry,
	// clh
	ClearFlagHalfCarry,
	// sec
	SetFlagCarry,
	// clc
	ClearFlagCarry,
	// clo
	ClearFlagOverflow,
	// sez
	SetFlagZero,
	// clz
	ClearFlagZero,
	// jmp a/(a)/zb/(zb)/ipr
	Jump(MemArg),
	// jmpx (a)/(zb)
	JumpX(MemArg),
	// jmpy (a)/(zb)
	JumpY(MemArg),
	// call a/zb
	Call(MemArg),
	// cmpi #/ir ir
	CmpInt(ALArg, Reg),
	// beq ipr
	BranchEqual(MemArg),
	// bne ipr
	BranchNotEqual(MemArg),
	// blu ipr
	BranchLessUnsigned(MemArg),
	// bls ipr
	BranchLessSigned(MemArg),
	// bgu ipr
	BranchGreaterUnsigned(MemArg),
	// bgs ipr
	BranchGreaterSigned(MemArg),
	// bleu ipr
	BranchLessOrEqualUnsigned(MemArg),
	// bles ipr
	BranchLessOrEqualSigned(MemArg),
	// bgeu ipr
	BranchGreaterOrEqualUnsigned(MemArg),
	// bges ipr
	BranchGreaterOrEqualSigned(MemArg),
	// bcs ipr
	BranchCarrySet(MemArg),
	// bcc ipr
	BranchCarryClear(MemArg),
	// bhs ipr
	BranchHalfCarrySet(MemArg),
	// bhc ipr
	BranchHalfCarryClear(MemArg),
	// bos ipr
	BranchOverflowSet(MemArg),
	// boc ipr
	BranchOverflowClear(MemArg),
	// inc ir
	IncInt(Reg),
	// ixbne # ipr
	IncXBranchNotEqual(ImmArg, MemArg),
	// iybne # ipr
	IncYBranchNotEqual(ImmArg, MemArg),
	// dec ir
	DecInt(Reg),
	// dxbnz ipr
	DecXBranchNotZero(MemArg),
	// dybnz ipr
	DecYBranchNotZero(MemArg),
	// bit a/zb/dp/p r
	TestBits(MemArg, Reg),
	// bsf ipr
	BranchStackFull(MemArg),
	// bse ipr
	BranchStackEmpty(MemArg),
	// bdp ipr
	BranchDataParity(MemArg),
	// cirq (a)
	CallIRQ(MemArg),
	// ret
	Return,
	// rti
	ReturnInterrupt,
	// brk
	Break,
	// wait
	Wait
}

// directives
pub enum Dir {
	// .org $FE0000
	SetAddress(u32),
	// .byte #42
	PutByte(ImmArg),
	// .word #$BEEF
	PutWord(ImmArg),
	// .vec $001337 or .vec label
	PutVector(MemArg),
	// .str "ASCII String"
	PutString(Vec<u8>),
	// .strz "ASCII String"
	PutStringNullTerm(Vec<u8>),
	// .incsrc & .incbin, bool field is true if binary, false if source
	Include(PathBuf, bool)
}

// immediate values
pub enum Imm {
	// 8-bit byte immediate
	Byte(u8),
	// 16-bit word immediate
	Word(u16)
}

// registers
pub enum Reg {
	IntegerA,		// iA	(16-bit)
	IntegerALow,	// iAL	(8-bit, low byte of iA)
	IntegerAHigh,	// iAH	(8-bit, high byte of iA)
	IntegerB,		// iB	(16-bit)
	IntegerBLow,	// iBL	(8-bit, low byte of iB)
	IntegerBHigh,	// iBH	(8-bit, high byte of iB)
	IntegerX,		// iX	(16-bit)
	IntegerY		// iY	(16-bit)
}

// used by the MVR (move to register) instruction
pub enum MoveArg {
	Immediate(Imm),
	Address(Addr),
	Identifier(String)
}

// used by any instruction which can take an address (MVM, MVMX, MVMY, SMZ, SMZX, SMZY, JMP, JMPX, JMPY, CALL, Branch instructions)
pub enum MemArg {
	Address(Addr),
	Identifier(String)
}

pub enum ImmArg {
	Immediate(Imm),
	Identifier(String)
}

// used by arithmetic logic instructions which can take an immediate or register as first argument
pub enum ALArg {
	Immediate(Imm),
	Register(Reg),
	Identifier(String)
}

pub enum Symbol {
	Immediate(Imm),
	Address(Addr),
	// future-proofing for when operators are added
	Identifier(String)
}

//--> Functions <--

impl FileAsm {
	pub fn parse(p: &Path, t: TokenStream) -> FileAsmResult {
		let mut asm = FileAsm { symbols: HashMap::new(), labels: HashMap::new(), code: HashMap::new() };
		let mut errs = Vec::new();
		let mut wrns = Vec::new();

		// iterate over all the lines in the file, enumerated for better error reporting.
		'lines: for (lno, tline) in t.split(|tok| if let Token::Newline = tok { true } else { false }).enumerate() {
			// skip if the line is empty
			if tline.is_empty() { continue 'lines; }

			// iterator over the tokens in the line
			let mut tl = tline.iter();

			// to allow for labels and instructions on the same line
			'toks: loop {
				match tl.next() {
					Some(t0) => if let Token::Word(w) = t0 {
						match w {
							// Instructions
							Wrd::NoOperation => {
								asm.code.insert(lno, Code::Instruction(Inst::NoOp));
								break 'toks;
							},
							Wrd::MoveToRegister => match tl.next() {
								Some(t1) => {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = MoveArg::Immediate(Imm::Byte(0));

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => src = MoveArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => src = MoveArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> MVR instruction expected an immediate or address first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Address(a) => src = MoveArg::Address(a.clone()),
										Token::Word(Wrd::Identifier(id)) => src = MoveArg::Identifier(id.clone()),
										_ => {
											errs.push(format!("<ERR! {}:{}> MVR instruction expected an immediate or address first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t1 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerA,
												_ => {
													errs.push(format!("<ERR! {}:{}> MVR instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::MoveToReg(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> MVR instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> MVR instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> MVR instruction expected an immediate or address first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::MoveToMemory => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = Reg::IntegerA;

									match w {
										Wrd::IntegerRegisterA => {}, // don't have to do anything for this one, it's already in there!
										Wrd::IntegerRegisterALow => src = Reg::IntegerALow,
										Wrd::IntegerRegisterAHigh => src = Reg::IntegerAHigh,
										Wrd::IntegerRegisterB => src = Reg::IntegerB,
										Wrd::IntegerRegisterBLow => src = Reg::IntegerBLow,
										Wrd::IntegerRegisterBHigh => src = Reg::IntegerBHigh,
										Wrd::IntegerRegisterX => src = Reg::IntegerX,
										Wrd::IntegerRegisterY => src = Reg::IntegerY,
										_ => {
											errs.push(format!("<ERR! {}:{}> MVM instruction expected a register first, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => {
											let dst: MemArg;

											match t2 {
												Token::Address(a) => match a {
													Addr::Absolute(_, _) | Addr::ZeroBank(_, _) | Addr::Port(_) | Addr::DirectPage(_) => dst = MemArg::Address(a.clone()),
													Addr::PointerRelative(_, ptr) => match ptr {
														Ptr::Instruction => {
															errs.push(format!("<ERR! {}:{}> MVM instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
															continue 'lines;
														},
														Ptr::Stack => {
															errs.push(format!("<ERR! {}:{}> MVM instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
															continue 'lines;
														}
													}
												},
												Token::Word(Wrd::Identifier(id)) => dst = MemArg::Identifier(id.clone()),
												_ => {
													errs.push(format!("<ERR! {}:{}> MVM instruction expected an address second, found {} instead.", p.display(), lno, t2));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::MoveToMem(src, dst)));
											break 'toks;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> MVM instruction expected an address second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> MVM instruction expected a register first, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> MVM instruction expected a register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::MoveToRegisterX => match tl.next() {
								Some(t1) => {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = MemArg::Address(Addr::DirectPage(0));

									match t1 {
										Token::Address(a) => match a {
											Addr::Absolute(_, i) => if !i { src = MemArg::Address(a.clone()); } else {
												errs.push(format!("<ERR! {}:{}> MVRX instruction does not support Indirect Absolute addressing.", p.display(), lno));
												continue 'lines;
											},
											Addr::ZeroBank(_, i) => if !i { src = MemArg::Address(a.clone()); } else {
												errs.push(format!("<ERR! {}:{}> MVRX instruction does not support Indirect Zero Bank addressing.", p.display(), lno));
												continue 'lines;
											},
											Addr::Port(_) | Addr::DirectPage(_) => src = MemArg::Address(a.clone()),
											Addr::PointerRelative(_, ptr) => match ptr {
												Ptr::Instruction => {
													errs.push(format!("<ERR! {}:{}> MVRX instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
													continue 'lines;
												},
												Ptr::Stack => {
													errs.push(format!("<ERR! {}:{}> MVRX instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
													continue 'lines;
												}
											}
										},
										Token::Word(Wrd::Identifier(id)) => src = MemArg::Identifier(id.clone()),
										_ => {
											errs.push(format!("<ERR! {}:{}> MVRX instruction expected an address first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t1 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerA,
												_ => {
													errs.push(format!("<ERR! {}:{}> MVRX instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::MoveToRegX(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> MVRX instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> MVRX instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> MVRX instruction expected an address first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::MoveToRegisterY => match tl.next() {
								Some(t1) => {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = MemArg::Address(Addr::DirectPage(0));

									match t1 {
										Token::Address(a) => match a {
											Addr::Absolute(_, i) => if !i { src = MemArg::Address(a.clone()); } else {
												errs.push(format!("<ERR! {}:{}> MVRY instruction does not support Indirect Absolute addressing.", p.display(), lno));
												continue 'lines;
											},
											Addr::ZeroBank(_, i) => if !i { src = MemArg::Address(a.clone()); } else {
												errs.push(format!("<ERR! {}:{}> MVRY instruction does not support Indirect Zero Bank addressing.", p.display(), lno));
												continue 'lines;
											},
											Addr::Port(_) | Addr::DirectPage(_) => src = MemArg::Address(a.clone()),
											Addr::PointerRelative(_, ptr) => match ptr {
												Ptr::Instruction => {
													errs.push(format!("<ERR! {}:{}> MVRY instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
													continue 'lines;
												},
												Ptr::Stack => {
													errs.push(format!("<ERR! {}:{}> MVRY instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
													continue 'lines;
												}
											}
										},
										Token::Word(Wrd::Identifier(id)) => src = MemArg::Identifier(id.clone()),
										_ => {
											errs.push(format!("<ERR! {}:{}> MVRY instruction expected an address first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t1 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerA,
												_ => {
													errs.push(format!("<ERR! {}:{}> MVRY instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::MoveToRegY(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> MVRY instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> MVRY instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> MVRY instruction expected an address first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::MoveToMemoryX => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = Reg::IntegerA;

									match w {
										Wrd::IntegerRegisterA => {}, // don't have to do anything for this one, it's already in there!
										Wrd::IntegerRegisterALow => src = Reg::IntegerALow,
										Wrd::IntegerRegisterAHigh => src = Reg::IntegerAHigh,
										Wrd::IntegerRegisterB => src = Reg::IntegerB,
										Wrd::IntegerRegisterBLow => src = Reg::IntegerBLow,
										Wrd::IntegerRegisterBHigh => src = Reg::IntegerBHigh,
										Wrd::IntegerRegisterX => src = Reg::IntegerX,
										Wrd::IntegerRegisterY => src = Reg::IntegerY,
										_ => {
											errs.push(format!("<ERR! {}:{}> MVMX instruction expected a register first, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => {
											let dst: MemArg;

											match t2 {
												Token::Address(a) => match a {
													Addr::Absolute(_, i) => if !i { dst = MemArg::Address(a.clone()); } else {
														errs.push(format!("<ERR! {}:{}> MVMX instruction does not support Indirect Absolute addressing.", p.display(), lno));
														continue 'lines;
													},
													Addr::ZeroBank(_, i) => if !i { dst = MemArg::Address(a.clone()); } else {
														errs.push(format!("<ERR! {}:{}> MVMX instruction does not support Indirect Zero Bank addressing.", p.display(), lno));
														continue 'lines;
													},
													Addr::Port(_) | Addr::DirectPage(_) => dst = MemArg::Address(a.clone()),
													Addr::PointerRelative(_, ptr) => match ptr {
														Ptr::Instruction => {
															errs.push(format!("<ERR! {}:{}> MVMX instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
															continue 'lines;
														},
														Ptr::Stack => {
															errs.push(format!("<ERR! {}:{}> MVMX instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
															continue 'lines;
														}
													}
												},
												Token::Word(Wrd::Identifier(id)) => dst = MemArg::Identifier(id.clone()),
												_ => {
													errs.push(format!("<ERR! {}:{}> MVMX instruction expected an address second, found {} instead.", p.display(), lno, t2));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::MoveToMemX(src, dst)));
											break 'toks;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> MVMX instruction expected an address second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> MVMX instruction expected a register first, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> MVMX instruction expected a register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::MoveToMemoryY => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = Reg::IntegerA;

									match w {
										Wrd::IntegerRegisterA => {}, // don't have to do anything for this one, it's already in there!
										Wrd::IntegerRegisterALow => src = Reg::IntegerALow,
										Wrd::IntegerRegisterAHigh => src = Reg::IntegerAHigh,
										Wrd::IntegerRegisterB => src = Reg::IntegerB,
										Wrd::IntegerRegisterBLow => src = Reg::IntegerBLow,
										Wrd::IntegerRegisterBHigh => src = Reg::IntegerBHigh,
										Wrd::IntegerRegisterX => src = Reg::IntegerX,
										Wrd::IntegerRegisterY => src = Reg::IntegerY,
										_ => {
											errs.push(format!("<ERR! {}:{}> MVMY instruction expected a register first, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => {
											let dst: MemArg;

											match t2 {
												Token::Address(a) => match a {
													Addr::Absolute(_, i) => if !i { dst = MemArg::Address(a.clone()); } else {
														errs.push(format!("<ERR! {}:{}> MVMY instruction does not support Indirect Absolute addressing.", p.display(), lno));
														continue 'lines;
													},
													Addr::ZeroBank(_, i) => if !i { dst = MemArg::Address(a.clone()); } else {
														errs.push(format!("<ERR! {}:{}> MVMY instruction does not support Indirect Zero Bank addressing.", p.display(), lno));
														continue 'lines;
													},
													Addr::Port(_) | Addr::DirectPage(_) => dst = MemArg::Address(a.clone()),
													Addr::PointerRelative(_, ptr) => match ptr {
														Ptr::Instruction => {
															errs.push(format!("<ERR! {}:{}> MVMY instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
															continue 'lines;
														},
														Ptr::Stack => {
															errs.push(format!("<ERR! {}:{}> MVMY instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
															continue 'lines;
														}
													}
												},
												Token::Word(Wrd::Identifier(id)) => dst = MemArg::Identifier(id.clone()),
												_ => {
													errs.push(format!("<ERR! {}:{}> MVMY instruction expected an address second, found {} instead.", p.display(), lno, t2));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::MoveToMemY(src, dst)));
											break 'toks;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> MVMY instruction expected an address second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> MVMY instruction expected a register first, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> MVMY instruction expected a register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::SetRegisterToZero => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									match w {
										Wrd::IntegerRegisterA => {
											asm.code.insert(lno, Code::Instruction(Inst::SetRegZero(Reg::IntegerA)));
											break 'toks;
										},
										Wrd::IntegerRegisterALow => {
											asm.code.insert(lno, Code::Instruction(Inst::SetRegZero(Reg::IntegerALow)));
											break 'toks;
										},
										Wrd::IntegerRegisterAHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::SetRegZero(Reg::IntegerAHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterB => {
											asm.code.insert(lno, Code::Instruction(Inst::SetRegZero(Reg::IntegerB)));
											break 'toks;
										},
										Wrd::IntegerRegisterBLow => {
											asm.code.insert(lno, Code::Instruction(Inst::SetRegZero(Reg::IntegerBLow)));
											break 'toks;
										},
										Wrd::IntegerRegisterBHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::SetRegZero(Reg::IntegerBHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterX => {
											asm.code.insert(lno, Code::Instruction(Inst::SetRegZero(Reg::IntegerX)));
											break 'toks;
										},
										Wrd::IntegerRegisterY => {
											asm.code.insert(lno, Code::Instruction(Inst::SetRegZero(Reg::IntegerY)));
											break 'toks;
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> SRZ instruction expected a register, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> SRZ instruction expected a register, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SRZ instruction expected a register, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::SetMemoryToZero => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => match a {
										Addr::Absolute(_, _) | Addr::ZeroBank(_, _) | Addr::Port(_) | Addr::DirectPage(_) => {
											asm.code.insert(lno, Code::Instruction(Inst::SetMemZero(MemArg::Address(a.clone()))));
											break 'toks;
										},
										Addr::PointerRelative(_, ptr) => match ptr {
											Ptr::Instruction => {
												errs.push(format!("<ERR! {}:{}> SMZ instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											},
											Ptr::Stack => {
												errs.push(format!("<ERR! {}:{}> SMZ instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											}
										}
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::SetMemZero(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> SMZ instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SMZ instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::SetMemoryToZeroX => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => match a {
										Addr::Absolute(_, i) => if !i {
											asm.code.insert(lno, Code::Instruction(Inst::SetMemZeroX(MemArg::Address(a.clone()))));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> SMZX instruction does not support Indirect Absolute addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::ZeroBank(_, i) => if !i {
											asm.code.insert(lno, Code::Instruction(Inst::SetMemZeroX(MemArg::Address(a.clone()))));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> SMZX instruction does not support Indirect Zero Bank addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::Port(_) | Addr::DirectPage(_) => {
											asm.code.insert(lno, Code::Instruction(Inst::SetMemZeroX(MemArg::Address(a.clone()))));
											break 'toks;
										},
										Addr::PointerRelative(_, ptr) => match ptr {
											Ptr::Instruction => {
												errs.push(format!("<ERR! {}:{}> SMZX instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											},
											Ptr::Stack => {
												errs.push(format!("<ERR! {}:{}> SMZX instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											}
										}
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::SetMemZeroX(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> SMZX instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SMZX instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::SetMemoryToZeroY => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => match a {
										Addr::Absolute(_, i) => if !i {
											asm.code.insert(lno, Code::Instruction(Inst::SetMemZeroY(MemArg::Address(a.clone()))));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> SMZY instruction does not support Indirect Absolute addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::ZeroBank(_, i) => if !i {
											asm.code.insert(lno, Code::Instruction(Inst::SetMemZeroY(MemArg::Address(a.clone()))));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> SMZY instruction does not support Indirect Zero Bank addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::Port(_) | Addr::DirectPage(_) => {
											asm.code.insert(lno, Code::Instruction(Inst::SetMemZeroY(MemArg::Address(a.clone()))));
											break 'toks;
										},
										Addr::PointerRelative(_, ptr) => match ptr {
											Ptr::Instruction => {
												errs.push(format!("<ERR! {}:{}> SMZY instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											},
											Ptr::Stack => {
												errs.push(format!("<ERR! {}:{}> SMZY instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											}
										}
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::SetMemZeroY(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> SMZY instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SMZY instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::HaltAndCatchFire => {
								asm.code.insert(lno, Code::Instruction(Inst::HaltCatchFire));
								break 'toks;
							},
							Wrd::TransferRegisterContents => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									let src: Reg;

									match w {
										Wrd::IntegerRegisterA => src = Reg::IntegerA,
										Wrd::IntegerRegisterALow => src = Reg::IntegerALow,
										Wrd::IntegerRegisterAHigh => src = Reg::IntegerAHigh,
										Wrd::IntegerRegisterB => src = Reg::IntegerB,
										Wrd::IntegerRegisterBLow => src = Reg::IntegerBLow,
										Wrd::IntegerRegisterBHigh => src = Reg::IntegerBHigh,
										Wrd::IntegerRegisterX => src = Reg::IntegerX,
										Wrd::IntegerRegisterY => src = Reg::IntegerY,
										_ => {
											errs.push(format!("<ERR! {}:{}> TRC instruction expected a register first, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> TRC instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::TrxRegContents(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> TRC instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> TRC instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> TRC instruction expected a register first, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> TRC instruction expected a register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::SwapRegisterContents => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									let r0: Reg;

									match w {
										Wrd::IntegerRegisterA => r0 = Reg::IntegerA,
										Wrd::IntegerRegisterALow => r0 = Reg::IntegerALow,
										Wrd::IntegerRegisterAHigh => r0 = Reg::IntegerAHigh,
										Wrd::IntegerRegisterB => r0 = Reg::IntegerB,
										Wrd::IntegerRegisterBLow => r0 = Reg::IntegerBLow,
										Wrd::IntegerRegisterBHigh => r0 = Reg::IntegerBHigh,
										Wrd::IntegerRegisterX => r0 = Reg::IntegerX,
										Wrd::IntegerRegisterY => r0 = Reg::IntegerY,
										_ => {
											errs.push(format!("<ERR! {}:{}> SRC instruction expected a register first, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let r1: Reg;

											match w {
												Wrd::IntegerRegisterA => r1 = Reg::IntegerA,
												Wrd::IntegerRegisterALow => r1 = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => r1 = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => r1 = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => r1 = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => r1 = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => r1 = Reg::IntegerX,
												Wrd::IntegerRegisterY => r1 = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> SRC instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::SwpRegContents(r0, r1)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> SRC instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> SRC instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> SRC instruction expected a register first, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SRC instruction expected a register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::StoreStackPointerToX => {
								asm.code.insert(lno, Code::Instruction(Inst::StoreStackPtrX));
								break 'toks;
							},
							Wrd::GetStackPointerFromX => {
								asm.code.insert(lno, Code::Instruction(Inst::GetStackPtrX));
								break 'toks;
							},
							Wrd::SetStackBank => match tl.next() {
								Some(t1) => match t1 {
									Token::Constant(Const::Byte(b)) => {
										asm.code.insert(lno, Code::Instruction(Inst::SetStackBank(ImmArg::Immediate(Imm::Byte(*b)))));
										break 'toks;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::SetStackBank(ImmArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> SSB instruction expected a byte immediate, found {} instead.", p.display(), lno, t1));
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SSB instruction expected a byte immediate, found nothing instead.", p.display(), lno));
								}
							},
							Wrd::SetDirectPage => match tl.next() {
								Some(t1) => match t1 {
									Token::Constant(Const::Byte(b)) => {
										asm.code.insert(lno, Code::Instruction(Inst::SetDirectPage(ImmArg::Immediate(Imm::Byte(*b)))));
										break 'toks;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::SetDirectPage(ImmArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> SDP instruction expected a byte immediate, found {} instead.", p.display(), lno, t1));
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SDP instruction expected a byte immediate, found nothing instead.", p.display(), lno));
								}
							},
							Wrd::PushToStack => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									match w {
										Wrd::IntegerRegisterA => {
											asm.code.insert(lno, Code::Instruction(Inst::Push(Reg::IntegerA)));
											break 'toks;
										},
										Wrd::IntegerRegisterALow => {
											asm.code.insert(lno, Code::Instruction(Inst::Push(Reg::IntegerALow)));
											break 'toks;
										},
										Wrd::IntegerRegisterAHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::Push(Reg::IntegerAHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterB => {
											asm.code.insert(lno, Code::Instruction(Inst::Push(Reg::IntegerB)));
											break 'toks;
										},
										Wrd::IntegerRegisterBLow => {
											asm.code.insert(lno, Code::Instruction(Inst::Push(Reg::IntegerBLow)));
											break 'toks;
										},
										Wrd::IntegerRegisterBHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::Push(Reg::IntegerBHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterX => {
											asm.code.insert(lno, Code::Instruction(Inst::Push(Reg::IntegerX)));
											break 'toks;
										},
										Wrd::IntegerRegisterY => {
											asm.code.insert(lno, Code::Instruction(Inst::Push(Reg::IntegerY)));
											break 'toks;
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> PUSH instruction expected a register, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> PUSH instruction expected a register, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> PUSH instruction expected a register, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::PopFromStack => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									match w {
										Wrd::IntegerRegisterA => {
											asm.code.insert(lno, Code::Instruction(Inst::Pop(Reg::IntegerA)));
											break 'toks;
										},
										Wrd::IntegerRegisterALow => {
											asm.code.insert(lno, Code::Instruction(Inst::Pop(Reg::IntegerALow)));
											break 'toks;
										},
										Wrd::IntegerRegisterAHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::Pop(Reg::IntegerAHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterB => {
											asm.code.insert(lno, Code::Instruction(Inst::Pop(Reg::IntegerB)));
											break 'toks;
										},
										Wrd::IntegerRegisterBLow => {
											asm.code.insert(lno, Code::Instruction(Inst::Pop(Reg::IntegerBLow)));
											break 'toks;
										},
										Wrd::IntegerRegisterBHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::Pop(Reg::IntegerBHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterX => {
											asm.code.insert(lno, Code::Instruction(Inst::Pop(Reg::IntegerX)));
											break 'toks;
										},
										Wrd::IntegerRegisterY => {
											asm.code.insert(lno, Code::Instruction(Inst::Pop(Reg::IntegerY)));
											break 'toks;
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> POP instruction expected a register, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> POP instruction expected a register, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> POP instruction expected a register, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::PushIntegerFlagsToStack => {
								asm.code.insert(lno, Code::Instruction(Inst::PushIntFlags));
								break 'toks;
							},
							Wrd::PopIntegerFlagsFromStack => {
								asm.code.insert(lno, Code::Instruction(Inst::PopIntFlags));
								break 'toks;
							},
							Wrd::And => match tl.next() {
								Some(t1) => {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = ALArg::Immediate(Imm::Byte(0));

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => src = ALArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => src = ALArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> AND instruction expected an immediate or register first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(w) => match w {
											Wrd::IntegerRegisterA => src = ALArg::Register(Reg::IntegerA),
											Wrd::IntegerRegisterALow => src = ALArg::Register(Reg::IntegerALow),
											Wrd::IntegerRegisterAHigh => src = ALArg::Register(Reg::IntegerAHigh),
											Wrd::IntegerRegisterB => src = ALArg::Register(Reg::IntegerB),
											Wrd::IntegerRegisterBLow => src = ALArg::Register(Reg::IntegerBLow),
											Wrd::IntegerRegisterBHigh => src = ALArg::Register(Reg::IntegerBHigh),
											Wrd::IntegerRegisterX => src = ALArg::Register(Reg::IntegerX),
											Wrd::IntegerRegisterY => src = ALArg::Register(Reg::IntegerY),
											Wrd::Identifier(id) => src = ALArg::Identifier(id.clone()),
											_ => {
												errs.push(format!("<ERR! {}:{}> AND instruction expected an immediate or register first, found {} instead.", p.display(), lno, w));
												continue 'lines;
											}
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> AND instruction expected an immediate or register first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> AND instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::And(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> AND instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> AND instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> AND instruction expected an immediate or register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::Or => match tl.next() {
								Some(t1) => {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = ALArg::Immediate(Imm::Byte(0));

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => src = ALArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => src = ALArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> OR instruction expected an immediate or register first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(w) => match w {
											Wrd::IntegerRegisterA => src = ALArg::Register(Reg::IntegerA),
											Wrd::IntegerRegisterALow => src = ALArg::Register(Reg::IntegerALow),
											Wrd::IntegerRegisterAHigh => src = ALArg::Register(Reg::IntegerAHigh),
											Wrd::IntegerRegisterB => src = ALArg::Register(Reg::IntegerB),
											Wrd::IntegerRegisterBLow => src = ALArg::Register(Reg::IntegerBLow),
											Wrd::IntegerRegisterBHigh => src = ALArg::Register(Reg::IntegerBHigh),
											Wrd::IntegerRegisterX => src = ALArg::Register(Reg::IntegerX),
											Wrd::IntegerRegisterY => src = ALArg::Register(Reg::IntegerY),
											Wrd::Identifier(id) => src = ALArg::Identifier(id.clone()),
											_ => {
												errs.push(format!("<ERR! {}:{}> OR instruction expected an immediate or register first, found {} instead.", p.display(), lno, w));
												continue 'lines;
											}
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> OR instruction expected an immediate or register first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> OR instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::Or(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> OR instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> OR instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> OR instruction expected an immediate or register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::ExclusiveOr => match tl.next() {
								Some(t1) => {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = ALArg::Immediate(Imm::Byte(0));

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => src = ALArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => src = ALArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> EOR instruction expected an immediate or register first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(w) => match w {
											Wrd::IntegerRegisterA => src = ALArg::Register(Reg::IntegerA),
											Wrd::IntegerRegisterALow => src = ALArg::Register(Reg::IntegerALow),
											Wrd::IntegerRegisterAHigh => src = ALArg::Register(Reg::IntegerAHigh),
											Wrd::IntegerRegisterB => src = ALArg::Register(Reg::IntegerB),
											Wrd::IntegerRegisterBLow => src = ALArg::Register(Reg::IntegerBLow),
											Wrd::IntegerRegisterBHigh => src = ALArg::Register(Reg::IntegerBHigh),
											Wrd::IntegerRegisterX => src = ALArg::Register(Reg::IntegerX),
											Wrd::IntegerRegisterY => src = ALArg::Register(Reg::IntegerY),
											Wrd::Identifier(id) => src = ALArg::Identifier(id.clone()),
											_ => {
												errs.push(format!("<ERR! {}:{}> EOR instruction expected an immediate or register first, found {} instead.", p.display(), lno, w));
												continue 'lines;
											}
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> EOR instruction expected an immediate or register first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> EOR instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::ExclOr(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> EOR instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> EOR instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> EOR instruction expected an immediate or register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::AddIntegers => match tl.next() {
								Some(t1) => {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = ALArg::Immediate(Imm::Byte(0));

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => src = ALArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => src = ALArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> ADDI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(w) => match w {
											Wrd::IntegerRegisterA => src = ALArg::Register(Reg::IntegerA),
											Wrd::IntegerRegisterALow => src = ALArg::Register(Reg::IntegerALow),
											Wrd::IntegerRegisterAHigh => src = ALArg::Register(Reg::IntegerAHigh),
											Wrd::IntegerRegisterB => src = ALArg::Register(Reg::IntegerB),
											Wrd::IntegerRegisterBLow => src = ALArg::Register(Reg::IntegerBLow),
											Wrd::IntegerRegisterBHigh => src = ALArg::Register(Reg::IntegerBHigh),
											Wrd::IntegerRegisterX => src = ALArg::Register(Reg::IntegerX),
											Wrd::IntegerRegisterY => src = ALArg::Register(Reg::IntegerY),
											Wrd::Identifier(id) => src = ALArg::Identifier(id.clone()),
											_ => {
												errs.push(format!("<ERR! {}:{}> ADDI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, w));
												continue 'lines;
											}
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> ADDI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> ADDI instruction expected an integer register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::AddInt(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> ADDI instruction expected an integer register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> ADDI instruction expected an integer register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> ADDI instruction expected an immediate or integer register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::AddIntegersWithCarry => match tl.next() {
								Some(t1) => {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = ALArg::Immediate(Imm::Byte(0));

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => src = ALArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => src = ALArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> ADCI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(w) => match w {
											Wrd::IntegerRegisterA => src = ALArg::Register(Reg::IntegerA),
											Wrd::IntegerRegisterALow => src = ALArg::Register(Reg::IntegerALow),
											Wrd::IntegerRegisterAHigh => src = ALArg::Register(Reg::IntegerAHigh),
											Wrd::IntegerRegisterB => src = ALArg::Register(Reg::IntegerB),
											Wrd::IntegerRegisterBLow => src = ALArg::Register(Reg::IntegerBLow),
											Wrd::IntegerRegisterBHigh => src = ALArg::Register(Reg::IntegerBHigh),
											Wrd::IntegerRegisterX => src = ALArg::Register(Reg::IntegerX),
											Wrd::IntegerRegisterY => src = ALArg::Register(Reg::IntegerY),
											Wrd::Identifier(id) => src = ALArg::Identifier(id.clone()),
											_ => {
												errs.push(format!("<ERR! {}:{}> ADCI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, w));
												continue 'lines;
											}
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> ADCI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> ADCI instruction expected an integer register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::AddCarryInt(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> ADCI instruction expected an integer register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> ADCI instruction expected an integer register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> ADCI instruction expected an immediate or integer register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::SubtractIntegers => match tl.next() {
								Some(t1) => {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = ALArg::Immediate(Imm::Byte(0));

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => src = ALArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => src = ALArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> SUBI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(w) => match w {
											Wrd::IntegerRegisterA => src = ALArg::Register(Reg::IntegerA),
											Wrd::IntegerRegisterALow => src = ALArg::Register(Reg::IntegerALow),
											Wrd::IntegerRegisterAHigh => src = ALArg::Register(Reg::IntegerAHigh),
											Wrd::IntegerRegisterB => src = ALArg::Register(Reg::IntegerB),
											Wrd::IntegerRegisterBLow => src = ALArg::Register(Reg::IntegerBLow),
											Wrd::IntegerRegisterBHigh => src = ALArg::Register(Reg::IntegerBHigh),
											Wrd::IntegerRegisterX => src = ALArg::Register(Reg::IntegerX),
											Wrd::IntegerRegisterY => src = ALArg::Register(Reg::IntegerY),
											Wrd::Identifier(id) => src = ALArg::Identifier(id.clone()),
											_ => {
												errs.push(format!("<ERR! {}:{}> SUBI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, w));
												continue 'lines;
											}
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> SUBI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> SUBI instruction expected an integer register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::SubInt(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> SUBI instruction expected an integer register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> SUBI instruction expected an integer register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SUBI instruction expected an immediate or integer register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::SubtractIntegersWithCarry => match tl.next() {
								Some(t1) => {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = ALArg::Immediate(Imm::Byte(0));

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => src = ALArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => src = ALArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> SBCI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(w) => match w {
											Wrd::IntegerRegisterA => src = ALArg::Register(Reg::IntegerA),
											Wrd::IntegerRegisterALow => src = ALArg::Register(Reg::IntegerALow),
											Wrd::IntegerRegisterAHigh => src = ALArg::Register(Reg::IntegerAHigh),
											Wrd::IntegerRegisterB => src = ALArg::Register(Reg::IntegerB),
											Wrd::IntegerRegisterBLow => src = ALArg::Register(Reg::IntegerBLow),
											Wrd::IntegerRegisterBHigh => src = ALArg::Register(Reg::IntegerBHigh),
											Wrd::IntegerRegisterX => src = ALArg::Register(Reg::IntegerX),
											Wrd::IntegerRegisterY => src = ALArg::Register(Reg::IntegerY),
											Wrd::Identifier(id) => src = ALArg::Identifier(id.clone()),
											_ => {
												errs.push(format!("<ERR! {}:{}> SBCI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, w));
												continue 'lines;
											}
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> SBCI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> SBCI instruction expected an integer register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::SubCarryInt(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> SBCI instruction expected an integer register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> SBCI instruction expected an integer register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SBCI instruction expected an immediate or integer register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::Not => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									match w {
										Wrd::IntegerRegisterA => {
											asm.code.insert(lno, Code::Instruction(Inst::Not(Reg::IntegerA)));
											break 'toks;
										},
										Wrd::IntegerRegisterALow => {
											asm.code.insert(lno, Code::Instruction(Inst::Not(Reg::IntegerALow)));
											break 'toks;
										},
										Wrd::IntegerRegisterAHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::Not(Reg::IntegerAHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterB => {
											asm.code.insert(lno, Code::Instruction(Inst::Not(Reg::IntegerB)));
											break 'toks;
										},
										Wrd::IntegerRegisterBLow => {
											asm.code.insert(lno, Code::Instruction(Inst::Not(Reg::IntegerBLow)));
											break 'toks;
										},
										Wrd::IntegerRegisterBHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::Not(Reg::IntegerBHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterX => {
											asm.code.insert(lno, Code::Instruction(Inst::Not(Reg::IntegerX)));
											break 'toks;
										},
										Wrd::IntegerRegisterY => {
											asm.code.insert(lno, Code::Instruction(Inst::Not(Reg::IntegerY)));
											break 'toks;
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> NOT instruction expected a register, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> NOT instruction expected a register, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> NOT instruction expected a register, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::ShiftLeft => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									match w {
										Wrd::IntegerRegisterA => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftLeft(Reg::IntegerA)));
											break 'toks;
										},
										Wrd::IntegerRegisterALow => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftLeft(Reg::IntegerALow)));
											break 'toks;
										},
										Wrd::IntegerRegisterAHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftLeft(Reg::IntegerAHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterB => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftLeft(Reg::IntegerB)));
											break 'toks;
										},
										Wrd::IntegerRegisterBLow => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftLeft(Reg::IntegerBLow)));
											break 'toks;
										},
										Wrd::IntegerRegisterBHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftLeft(Reg::IntegerBHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterX => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftLeft(Reg::IntegerX)));
											break 'toks;
										},
										Wrd::IntegerRegisterY => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftLeft(Reg::IntegerY)));
											break 'toks;
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> SHL instruction expected a register, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> SHL instruction expected a register, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SHL instruction expected a register, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::ShiftRight => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									match w {
										Wrd::IntegerRegisterA => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftRight(Reg::IntegerA)));
											break 'toks;
										},
										Wrd::IntegerRegisterALow => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftRight(Reg::IntegerALow)));
											break 'toks;
										},
										Wrd::IntegerRegisterAHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftRight(Reg::IntegerAHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterB => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftRight(Reg::IntegerB)));
											break 'toks;
										},
										Wrd::IntegerRegisterBLow => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftRight(Reg::IntegerBLow)));
											break 'toks;
										},
										Wrd::IntegerRegisterBHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftRight(Reg::IntegerBHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterX => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftRight(Reg::IntegerX)));
											break 'toks;
										},
										Wrd::IntegerRegisterY => {
											asm.code.insert(lno, Code::Instruction(Inst::ShiftRight(Reg::IntegerY)));
											break 'toks;
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> SHR instruction expected a register, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> SHR instruction expected a register, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SHR instruction expected a register, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::RotateLeft => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									match w {
										Wrd::IntegerRegisterA => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateLeft(Reg::IntegerA)));
											break 'toks;
										},
										Wrd::IntegerRegisterALow => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateLeft(Reg::IntegerALow)));
											break 'toks;
										},
										Wrd::IntegerRegisterAHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateLeft(Reg::IntegerAHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterB => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateLeft(Reg::IntegerB)));
											break 'toks;
										},
										Wrd::IntegerRegisterBLow => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateLeft(Reg::IntegerBLow)));
											break 'toks;
										},
										Wrd::IntegerRegisterBHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateLeft(Reg::IntegerBHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterX => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateLeft(Reg::IntegerX)));
											break 'toks;
										},
										Wrd::IntegerRegisterY => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateLeft(Reg::IntegerY)));
											break 'toks;
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> ROL instruction expected a register, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> ROL instruction expected a register, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> ROL instruction expected a register, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::RotateRight => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									match w {
										Wrd::IntegerRegisterA => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateRight(Reg::IntegerA)));
											break 'toks;
										},
										Wrd::IntegerRegisterALow => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateRight(Reg::IntegerALow)));
											break 'toks;
										},
										Wrd::IntegerRegisterAHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateRight(Reg::IntegerAHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterB => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateRight(Reg::IntegerB)));
											break 'toks;
										},
										Wrd::IntegerRegisterBLow => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateRight(Reg::IntegerBLow)));
											break 'toks;
										},
										Wrd::IntegerRegisterBHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateRight(Reg::IntegerBHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterX => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateRight(Reg::IntegerX)));
											break 'toks;
										},
										Wrd::IntegerRegisterY => {
											asm.code.insert(lno, Code::Instruction(Inst::RotateRight(Reg::IntegerY)));
											break 'toks;
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> ROR instruction expected a register, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> ROR instruction expected a register, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> ROR instruction expected a register, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::PopulationCount => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									let src: Reg;

									match w {
										Wrd::IntegerRegisterA => src = Reg::IntegerA,
										Wrd::IntegerRegisterALow => src = Reg::IntegerALow,
										Wrd::IntegerRegisterAHigh => src = Reg::IntegerAHigh,
										Wrd::IntegerRegisterB => src = Reg::IntegerB,
										Wrd::IntegerRegisterBLow => src = Reg::IntegerBLow,
										Wrd::IntegerRegisterBHigh => src = Reg::IntegerBHigh,
										Wrd::IntegerRegisterX => src = Reg::IntegerX,
										Wrd::IntegerRegisterY => src = Reg::IntegerY,
										_ => {
											errs.push(format!("<ERR! {}:{}> PCNT instruction expected a register first, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> PCNT instruction expected a register second, found {} instead.", p.display(), lno, t2));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::PopCount(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> PCNT instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> PCNT instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> PCNT instruction expected a register first, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> PCNT instruction expected a register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::VacancyCount => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									let src: Reg;

									match w {
										Wrd::IntegerRegisterA => src = Reg::IntegerA,
										Wrd::IntegerRegisterALow => src = Reg::IntegerALow,
										Wrd::IntegerRegisterAHigh => src = Reg::IntegerAHigh,
										Wrd::IntegerRegisterB => src = Reg::IntegerB,
										Wrd::IntegerRegisterBLow => src = Reg::IntegerBLow,
										Wrd::IntegerRegisterBHigh => src = Reg::IntegerBHigh,
										Wrd::IntegerRegisterX => src = Reg::IntegerX,
										Wrd::IntegerRegisterY => src = Reg::IntegerY,
										_ => {
											errs.push(format!("<ERR! {}:{}> VCNT instruction expected a register first, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> VCNT instruction expected a register second, found {} instead.", p.display(), lno, t2));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::VacCount(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> VCNT instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> VCNT instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> VCNT instruction expected a register first, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> VCNT instruction expected a register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::Swizzle => match tl.next() {
								Some(t1) => {
									let mask: ImmArg;

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => mask = ImmArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => mask = ImmArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> SWZ instruction expected an immediate first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(Wrd::Identifier(id)) => mask = ImmArg::Identifier(id.clone()),
										_ => {
											errs.push(format!("<ERR! {}:{}> SWZ instruction expected an immediate first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> SWZ instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::Swizzle(mask, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> SWZ instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> SWZ instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SWZ instruction expected an immediate first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::SetBits => match tl.next() {
								Some(t1) => {
									let mask: ImmArg;

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => mask = ImmArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => mask = ImmArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> SET instruction expected an immediate first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(Wrd::Identifier(id)) => mask = ImmArg::Identifier(id.clone()),
										_ => {
											errs.push(format!("<ERR! {}:{}> SET instruction expected an immediate first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> SET instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::Set(mask, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> SET instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> SET instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> SET instruction expected an immediate first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::ClearBits => match tl.next() {
								Some(t1) => {
									let mask: ImmArg;

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => mask = ImmArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => mask = ImmArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> CLR instruction expected an immediate first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(Wrd::Identifier(id)) => mask = ImmArg::Identifier(id.clone()),
										_ => {
											errs.push(format!("<ERR! {}:{}> CLR instruction expected an immediate first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> CLR instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::Clear(mask, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> CLR instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> CLR instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> CLR instruction expected an immediate first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::EnableMaskableInterrupts => {
								asm.code.insert(lno, Code::Instruction(Inst::EnableIRQs));
								break 'toks;
							},
							Wrd::DisableMaskableInterrupts => {
								asm.code.insert(lno, Code::Instruction(Inst::DisableIRQs));
								break 'toks;
							},
							Wrd::SetDecimalFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::SetFlagDecimal));
								break 'toks;
							},
							Wrd::ClearDecimalFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::ClearFlagDecimal));
								break 'toks;
							},
							Wrd::SetSignFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::SetFlagSign));
								break 'toks;
							},
							Wrd::ClearSignFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::ClearFlagSign));
								break 'toks;
							},
							Wrd::SetNegativeFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::SetFlagNegative));
								break 'toks;
							},
							Wrd::ClearNegativeFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::ClearFlagNegative));
								break 'toks;
							},
							Wrd::SetHalfCarryFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::SetFlagHalfCarry));
								break 'toks;
							},
							Wrd::ClearHalfCarryFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::ClearFlagHalfCarry));
								break 'toks;
							},
							Wrd::SetCarryFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::SetFlagCarry));
								break 'toks;
							},
							Wrd::ClearCarryFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::ClearFlagCarry));
								break 'toks;
							},
							Wrd::SetZeroFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::SetFlagZero));
								break 'toks;
							},
							Wrd::ClearZeroFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::ClearFlagZero));
								break 'toks;
							},
							Wrd::ClearOverflowFlag => {
								asm.code.insert(lno, Code::Instruction(Inst::ClearFlagOverflow));
								break 'toks;
							},
							Wrd::Jump => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => match a {
										Addr::Absolute(_, _) | Addr::ZeroBank(_, _) => {
											asm.code.insert(lno, Code::Instruction(Inst::Jump(MemArg::Address(a.clone()))));
											break 'toks;
										},
										Addr::Port(_) => {
											errs.push(format!("<ERR! {}:{}> JMP instruction does not support Port addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::DirectPage(_) => {
											errs.push(format!("<ERR! {}:{}> JMP instruction does not support Direct Page addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::PointerRelative(_, ptr) => match ptr {
											Ptr::Instruction => {
												asm.code.insert(lno, Code::Instruction(Inst::Jump(MemArg::Address(a.clone()))));
												break 'toks;
											},
											Ptr::Stack => {
												errs.push(format!("<ERR! {}:{}> JMP instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											}
										}
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::Jump(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> JMP instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> JMP instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::JumpX => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => match a {
										Addr::Absolute(_, i) => if *i {
											asm.code.insert(lno, Code::Instruction(Inst::JumpX(MemArg::Address(a.clone()))));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> JMPX instruction does not support Absolute addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::ZeroBank(_, i) => if *i {
											asm.code.insert(lno, Code::Instruction(Inst::JumpX(MemArg::Address(a.clone()))));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> JMPX instruction does not support Zero Bank addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::Port(_) => {
											errs.push(format!("<ERR! {}:{}> JMPX instruction does not support Port addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::DirectPage(_) => {
											errs.push(format!("<ERR! {}:{}> JMPX instruction does not support Direct Page addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::PointerRelative(_, ptr) => match ptr {
											Ptr::Instruction => {
												errs.push(format!("<ERR! {}:{}> JMPX instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											},
											Ptr::Stack => {
												errs.push(format!("<ERR! {}:{}> JMPX instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											}
										}
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::JumpX(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> JMPX instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> JMPX instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::JumpY => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => match a {
										Addr::Absolute(_, i) => if *i {
											asm.code.insert(lno, Code::Instruction(Inst::JumpY(MemArg::Address(a.clone()))));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> JMPY instruction does not support Absolute addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::ZeroBank(_, i) => if *i {
											asm.code.insert(lno, Code::Instruction(Inst::JumpY(MemArg::Address(a.clone()))));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> JMPY instruction does not support Zero Bank addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::Port(_) => {
											errs.push(format!("<ERR! {}:{}> JMPY instruction does not support Port addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::DirectPage(_) => {
											errs.push(format!("<ERR! {}:{}> JMPY instruction does not support Direct Page addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::PointerRelative(_, ptr) => match ptr {
											Ptr::Instruction => {
												errs.push(format!("<ERR! {}:{}> JMPY instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											},
											Ptr::Stack => {
												errs.push(format!("<ERR! {}:{}> JMPY instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											}
										}
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::JumpY(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> JMPY instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> JMPY instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::CallSubroutine => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => match a {
										Addr::Absolute(_, i) => if !i {
											asm.code.insert(lno, Code::Instruction(Inst::Call(MemArg::Address(a.clone()))));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> CALL instruction does not support Indirect Absolute addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::ZeroBank(_, i) => if !i {
											asm.code.insert(lno, Code::Instruction(Inst::Call(MemArg::Address(a.clone()))));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> CALL instruction does not support Indirect Zero Bank addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::Port(_) => {
											errs.push(format!("<ERR! {}:{}> CALL instruction does not support Port addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::DirectPage(_) => {
											errs.push(format!("<ERR! {}:{}> CALL instruction does not support Direct Page addressing.", p.display(), lno));
											continue 'lines;
										},
										Addr::PointerRelative(_, ptr) => match ptr {
											Ptr::Instruction => {
												errs.push(format!("<ERR! {}:{}> CALL instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											},
											Ptr::Stack => {
												errs.push(format!("<ERR! {}:{}> CALL instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
												continue 'lines;
											}
										}
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::Call(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> CALL instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> CALL instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::CompareIntegers => match tl.next() {
								Some(t1) => {
									// dummy value here to prevent rustc complaining about possibly-uninitialized values
									// if no errors occur, this value will/should be replaced
									// if errors occur, the value will be dropped and not used
									let mut src = ALArg::Immediate(Imm::Byte(0));

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => src = ALArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => src = ALArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> CMPI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(w) => match w {
											Wrd::IntegerRegisterA => src = ALArg::Register(Reg::IntegerA),
											Wrd::IntegerRegisterALow => src = ALArg::Register(Reg::IntegerALow),
											Wrd::IntegerRegisterAHigh => src = ALArg::Register(Reg::IntegerAHigh),
											Wrd::IntegerRegisterB => src = ALArg::Register(Reg::IntegerB),
											Wrd::IntegerRegisterBLow => src = ALArg::Register(Reg::IntegerBLow),
											Wrd::IntegerRegisterBHigh => src = ALArg::Register(Reg::IntegerBHigh),
											Wrd::IntegerRegisterX => src = ALArg::Register(Reg::IntegerX),
											Wrd::IntegerRegisterY => src = ALArg::Register(Reg::IntegerY),
											Wrd::Identifier(id) => src = ALArg::Identifier(id.clone()),
											_ => {
												errs.push(format!("<ERR! {}:{}> CMPI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, w));
												continue 'lines;
											}
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> CMPI instruction expected an immediate or integer register first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let dst: Reg;

											match w {
												Wrd::IntegerRegisterA => dst = Reg::IntegerA,
												Wrd::IntegerRegisterALow => dst = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => dst = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => dst = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => dst = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => dst = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => dst = Reg::IntegerX,
												Wrd::IntegerRegisterY => dst = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> CMPI instruction expected an integer register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::CmpInt(src, dst)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> CMPI instruction expected an integer register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> CMPI instruction expected an integer register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> CMPI instruction expected an immediate or integer register first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfEqual => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchEqual(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BEQ instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchEqual(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BEQ instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BEQ instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfNotEqual => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchNotEqual(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BNE instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchNotEqual(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BNE instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BNE instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfLessThanUnsigned => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchLessUnsigned(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BLU instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchLessUnsigned(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BLU instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BLU instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfLessThanSigned => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchLessSigned(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BLS instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchLessSigned(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BLS instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BLS instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfGreaterThanUnsigned => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchGreaterUnsigned(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BGU instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchGreaterUnsigned(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BGU instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BGU instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfGreaterThanSigned => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchGreaterSigned(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BGS instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchGreaterSigned(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BGS instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BGS instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfLessThanOrEqualUnsigned => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchLessOrEqualUnsigned(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BLEU instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchLessOrEqualUnsigned(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BLEU instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BLEU instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfLessThanOrEqualSigned => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchLessOrEqualSigned(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BLES instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchLessOrEqualSigned(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BLES instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BLES instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfGreaterThanOrEqualUnsigned => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchGreaterOrEqualUnsigned(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BGEU instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchGreaterOrEqualUnsigned(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BGEU instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BGEU instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfGreaterThanOrEqualSigned => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchGreaterOrEqualSigned(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BGES instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchGreaterOrEqualSigned(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BGES instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BGES instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfCarrySet => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchCarrySet(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BCS instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchCarrySet(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BCS instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BCS instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfCarryClear => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchCarryClear(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BCC instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchCarryClear(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BCC instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BCC instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfHalfCarrySet => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchHalfCarrySet(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BHS instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchHalfCarrySet(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BHS instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BHS instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfHalfCarryClear => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchHalfCarryClear(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BHC instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchHalfCarryClear(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BHC instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BHC instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfOverflowSet => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchOverflowSet(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BOS instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchOverflowSet(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BOS instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BOS instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfOverflowClear => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchOverflowClear(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BOC instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchOverflowClear(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BOC instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BOC instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfStackFull => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchStackFull(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BSF instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchStackFull(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BSF instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BSF instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchIfStackEmpty => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchStackEmpty(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BSE instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchStackEmpty(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BSE instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BSE instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::Increment => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									match w {
										Wrd::IntegerRegisterA => {
											asm.code.insert(lno, Code::Instruction(Inst::IncInt(Reg::IntegerA)));
											break 'toks;
										},
										Wrd::IntegerRegisterALow => {
											asm.code.insert(lno, Code::Instruction(Inst::IncInt(Reg::IntegerALow)));
											break 'toks;
										},
										Wrd::IntegerRegisterAHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::IncInt(Reg::IntegerAHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterB => {
											asm.code.insert(lno, Code::Instruction(Inst::IncInt(Reg::IntegerB)));
											break 'toks;
										},
										Wrd::IntegerRegisterBLow => {
											asm.code.insert(lno, Code::Instruction(Inst::IncInt(Reg::IntegerBLow)));
											break 'toks;
										},
										Wrd::IntegerRegisterBHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::IncInt(Reg::IntegerBHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterX => {
											asm.code.insert(lno, Code::Instruction(Inst::IncInt(Reg::IntegerX)));
											break 'toks;
										},
										Wrd::IntegerRegisterY => {
											asm.code.insert(lno, Code::Instruction(Inst::IncInt(Reg::IntegerY)));
											break 'toks;
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> INC instruction expected an integer register, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> INC instruction expected an integer register, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> INC instruction expected an integer register, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::IncrementXBranchIfNotEqual => match tl.next() {
								Some(t1) => {
									let goal: ImmArg;

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => goal = ImmArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => goal = ImmArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> IXBNE instruction expected an immediate first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(Wrd::Identifier(id)) => goal = ImmArg::Identifier(id.clone()),
										_ => {
											errs.push(format!("<ERR! {}:{}> IXBNE instruction expected an immediate first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => {
											let branch: MemArg;

											match t2 {
												Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a { branch = MemArg::Address(a.clone()); } else {
													errs.push(format!("<ERR! {}:{}> IXBNE instruction only supports Instruction Pointer relative addressing.", p.display(), lno));
													continue 'lines;
												},
												Token::Word(Wrd::Identifier(id)) => branch = MemArg::Identifier(id.clone()),
												_ => {
													errs.push(format!("<ERR! {}:{}> IXBNE instruction expected an address second, found {} instead.", p.display(), lno, t2));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::IncXBranchNotEqual(goal, branch)));
											break 'toks;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> IXBNE instruction expected an address second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> IXBNE instruction expected an immediate first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::IncrementYBranchIfNotEqual => match tl.next() {
								Some(t1) => {
									let goal: ImmArg;

									match t1 {
										Token::Constant(c) => match c {
											Const::Byte(b) => goal = ImmArg::Immediate(Imm::Byte(*b)),
											Const::Word(w) => goal = ImmArg::Immediate(Imm::Word(*w)),
											_ => {
												errs.push(format!("<ERR! {}:{}> IYBNE instruction expected an immediate first, found {} instead.", p.display(), lno, c));
												continue 'lines;
											}
										},
										Token::Word(Wrd::Identifier(id)) => goal = ImmArg::Identifier(id.clone()),
										_ => {
											errs.push(format!("<ERR! {}:{}> IYBNE instruction expected an immediate first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => {
											let branch: MemArg;

											match t2 {
												Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a { branch = MemArg::Address(a.clone()); } else {
													errs.push(format!("<ERR! {}:{}> IYBNE instruction only supports Instruction Pointer relative addressing.", p.display(), lno));
													continue 'lines;
												},
												Token::Word(Wrd::Identifier(id)) => branch = MemArg::Identifier(id.clone()),
												_ => {
													errs.push(format!("<ERR! {}:{}> IYBNE instruction expected an address second, found {} instead.", p.display(), lno, t2));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::IncYBranchNotEqual(goal, branch)));
											break 'toks;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> IYBNE instruction expected an address second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> IYBNE instruction expected an immediate first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::Decrement => match tl.next() {
								Some(t1) => if let Token::Word(w) = t1 {
									match w {
										Wrd::IntegerRegisterA => {
											asm.code.insert(lno, Code::Instruction(Inst::DecInt(Reg::IntegerA)));
											break 'toks;
										},
										Wrd::IntegerRegisterALow => {
											asm.code.insert(lno, Code::Instruction(Inst::DecInt(Reg::IntegerALow)));
											break 'toks;
										},
										Wrd::IntegerRegisterAHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::DecInt(Reg::IntegerAHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterB => {
											asm.code.insert(lno, Code::Instruction(Inst::DecInt(Reg::IntegerB)));
											break 'toks;
										},
										Wrd::IntegerRegisterBLow => {
											asm.code.insert(lno, Code::Instruction(Inst::DecInt(Reg::IntegerBLow)));
											break 'toks;
										},
										Wrd::IntegerRegisterBHigh => {
											asm.code.insert(lno, Code::Instruction(Inst::DecInt(Reg::IntegerBHigh)));
											break 'toks;
										},
										Wrd::IntegerRegisterX => {
											asm.code.insert(lno, Code::Instruction(Inst::DecInt(Reg::IntegerX)));
											break 'toks;
										},
										Wrd::IntegerRegisterY => {
											asm.code.insert(lno, Code::Instruction(Inst::DecInt(Reg::IntegerY)));
											break 'toks;
										},
										_ => {
											errs.push(format!("<ERR! {}:{}> DEC instruction expected an integer register, found {} instead.", p.display(), lno, w));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> DEC instruction expected an integer register, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> DEC instruction expected an integer register, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::DecrementXBranchIfNotZero => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::DecXBranchNotZero(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> DXBNZ instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::DecXBranchNotZero(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> DXBNZ instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> DXBNZ instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::DecrementYBranchIfNotZero => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::DecYBranchNotZero(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> DYBNZ instruction only supports Instruction Pointer Relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::DecYBranchNotZero(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> DYBNZ instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> DYBNZ instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::TestBits => match tl.next() {
								Some(t1) => {
									let mask: MemArg;

									match t1 {
										Token::Address(a) => match a {
											Addr::Absolute(_, i) => if !i { mask = MemArg::Address(a.clone()); } else {
												errs.push(format!("<ERR! {}:{}> BIT instruction does not support Indirect Absolute addressing.", p.display(), lno));
												continue 'lines;
											},
											Addr::ZeroBank(_, i) => if !i { mask = MemArg::Address(a.clone()); } else {
												errs.push(format!("<ERR! {}:{}> BIT instruction does not support Indirect Zero Bank addressing.", p.display(), lno));
												continue 'lines;
											},
											Addr::Port(_) => mask = MemArg::Address(a.clone()),
											Addr::DirectPage(_) => mask = MemArg::Address(a.clone()),
											Addr::PointerRelative(_, ptr) => match ptr {
												Ptr::Instruction => {
													errs.push(format!("<ERR! {}:{}> BIT instruction does not support Instruction Pointer Relative addressing.", p.display(), lno));
													continue 'lines;
												},
												Ptr::Stack => {
													errs.push(format!("<ERR! {}:{}> BIT instruction does not support Stack Pointer Relative addressing.", p.display(), lno));
													continue 'lines;
												}
											}
										},
										Token::Word(Wrd::Identifier(id)) => mask = MemArg::Identifier(id.clone()),
										_ => {
											errs.push(format!("<ERR! {}:{}> BIT instruction expected an address first, found {} instead.", p.display(), lno, t1));
											continue 'lines;
										}
									}

									match tl.next() {
										Some(t2) => if let Token::Word(w) = t2 {
											let trg: Reg;

											match w {
												Wrd::IntegerRegisterA => trg = Reg::IntegerA,
												Wrd::IntegerRegisterALow => trg = Reg::IntegerALow,
												Wrd::IntegerRegisterAHigh => trg = Reg::IntegerAHigh,
												Wrd::IntegerRegisterB => trg = Reg::IntegerB,
												Wrd::IntegerRegisterBLow => trg = Reg::IntegerBLow,
												Wrd::IntegerRegisterBHigh => trg = Reg::IntegerBHigh,
												Wrd::IntegerRegisterX => trg = Reg::IntegerX,
												Wrd::IntegerRegisterY => trg = Reg::IntegerY,
												_ => {
													errs.push(format!("<ERR! {}:{}> BIT instruction expected a register second, found {} instead.", p.display(), lno, w));
													continue 'lines;
												}
											}

											asm.code.insert(lno, Code::Instruction(Inst::TestBits(mask, trg)));
											break 'toks;
										} else {
											errs.push(format!("<ERR! {}:{}> BIT instruction expected a register second, found {} instead.", p.display(), lno, t2));
											continue 'lines;
										},
										None => {
											errs.push(format!("<ERR! {}:{}> BIT instruction expected a register second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BIT instruction expected an address first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::BranchDataParity => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::PointerRelative(_, Ptr::Instruction) = a {
										asm.code.insert(lno, Code::Instruction(Inst::BranchDataParity(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> BDP instruction only supports Instruction Pointer relative addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::BranchDataParity(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> BDP instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> BDP instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::CallInterrupt => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(a) => if let Addr::Absolute(_, true) = a {
										asm.code.insert(lno, Code::Instruction(Inst::CallIRQ(MemArg::Address(a.clone()))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> CIRQ instruction only supports Indirect Absolute addressing.", p.display(), lno));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Instruction(Inst::CallIRQ(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> CIRQ instruction expected an address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> CIRQ instruction expected an address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::ReturnFromSubroutine => {
								asm.code.insert(lno, Code::Instruction(Inst::Return));
								break 'toks;
							},
							Wrd::ReturnFromInterrupt => {
								asm.code.insert(lno, Code::Instruction(Inst::ReturnInterrupt));
								break 'toks;
							},
							Wrd::Break => {
								asm.code.insert(lno, Code::Instruction(Inst::Break));
								break 'toks;
							},
							Wrd::WaitForInterrupt => {
								asm.code.insert(lno, Code::Instruction(Inst::Wait));
								break 'toks;
							},
							// Directives
							Wrd::Origin => match tl.next() {
								Some(t1) => if let Token::Address(Addr::Absolute(a, i)) = t1 {
									if !i {
										asm.code.insert(lno, Code::Directive(Dir::SetAddress(*a)));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> Origin directive expected an absolute address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								} else {
									errs.push(format!("<ERR! {}:{}> Origin directive expected an absolute address, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> Origin directive expected an absolute address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::DefineSymbol => match tl.next() {
								Some(t1) => if let Token::Word(Wrd::Identifier(id)) = t1 {
									// symbols must be unique, and will not be redefined.
									if asm.symbols.contains_key(id) {
										match asm.symbols.get(id).unwrap() {
											Symbol::Immediate(i) => match i {
												Imm::Byte(b) => wrns.push(format!("[WARN {}:{}] Symbol '{}' is already defined as byte immediate {}, will not be redefined.", p.display(), lno, id, b)),
												Imm::Word(w) => wrns.push(format!("[WARN {}:{}] Symbol '{}' is already defined as word immediate {}, will not be redefined.", p.display(), lno, id, w))
											},
											Symbol::Address(a) => wrns.push(format!("[WARN {}:{}] Symbol '{}' is already defined as {}, will not be redefined.", p.display(), lno, id, a)),
											Symbol::Identifier(_) => unreachable!()
										}
										continue 'lines;
									}
									
									match tl.next() {
										Some(t2) => match t2 {
											Token::Constant(c) => match c {
												Const::Byte(b) => {
													asm.symbols.insert(id.clone(), Symbol::Immediate(Imm::Byte(*b)));
													break 'toks;
												},
												Const::Word(w) => {
													asm.symbols.insert(id.clone(), Symbol::Immediate(Imm::Word(*w)));
													break 'toks;
												},
												_ => {
													errs.push(format!("<ERR! {}:{}> Define Symbol directive expected an immediate or address second, found {} instead.", p.display(), lno, c));
													continue 'lines;
												}
											},
											Token::Address(a) => {
												asm.symbols.insert(id.clone(), Symbol::Address(a.clone()));
												break 'toks;
											},
											Token::Word(Wrd::Identifier(_)) => {
												errs.push(format!("<ERR! {}:{}> Define Symbol directive will not support defining based on existing symbols or labels until operators are added.", p.display(), lno));
												continue 'lines;
											},
											_ => {
												errs.push(format!("<ERR! {}:{}> Define Symbol directive expected an immediate or address second, found {} instead.", p.display(), lno, t2));
												continue 'lines;
											}
										},
										None => {
											errs.push(format!("<ERR! {}:{}> Define Symbol directive expected an immediate or address second, found nothing instead.", p.display(), lno));
											continue 'lines;
										}
									}
								} else {
									errs.push(format!("<ERR! {}:{}> Define Symbol directive expected an identifier first, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> Define Symbol directive expected an identifier first, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::PlaceByte => match tl.next() {
								Some(t1) => match t1 {
									Token::Constant(Const::Byte(b)) => {
										asm.code.insert(lno, Code::Directive(Dir::PutByte(ImmArg::Immediate(Imm::Byte(*b)))));
										break 'toks;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Directive(Dir::PutByte(ImmArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> Place Byte directive expected a byte immediate, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> Place Byte directive expected a byte immediate, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::PlaceWord => match tl.next() {
								Some(t1) => match t1 {
									Token::Constant(Const::Word(w)) => {
										asm.code.insert(lno, Code::Directive(Dir::PutWord(ImmArg::Immediate(Imm::Word(*w)))));
										break 'toks;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Directive(Dir::PutWord(ImmArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> Place Word directive expected a word immediate, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> Place Word directive expected a word immediate, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::PlaceVector => match tl.next() {
								Some(t1) => match t1 {
									Token::Address(Addr::Absolute(a, i)) => if !i {
										asm.code.insert(lno, Code::Directive(Dir::PutVector(MemArg::Address(Addr::Absolute(*a, *i)))));
										break 'toks;
									} else {
										errs.push(format!("<ERR! {}:{}> Place Vector directive expected an absolute address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									},
									Token::Word(Wrd::Identifier(id)) => {
										asm.code.insert(lno, Code::Directive(Dir::PutVector(MemArg::Identifier(id.clone()))));
										break 'toks;
									},
									_ => {
										errs.push(format!("<ERR! {}:{}> Place Vector directive expected an absolute address, found {} instead.", p.display(), lno, t1));
										continue 'lines;
									}
								},
								None => {
									errs.push(format!("<ERR! {}:{}> Place Vector directive expected an absolute address, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::PlaceString => match tl.next() {
								Some(t1) => if let Token::Constant(Const::String(s)) = t1 {
									asm.code.insert(lno, Code::Directive(Dir::PutString(s.clone())));
									break 'toks;
								} else {
									errs.push(format!("<ERR! {}:{}> Place String directive expected a string literal, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> Place String directive expected a string literal, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::PlaceNullTerminatedString => match tl.next() {
								Some(t1) => if let Token::Constant(Const::String(s)) = t1 {
									asm.code.insert(lno, Code::Directive(Dir::PutStringNullTerm(s.clone())));
									break 'toks;
								} else {
									errs.push(format!("<ERR! {}:{}> Place Null-Terminated String directive expected a string literal, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> Place Null-Terminated String directive expected a string literal, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::IncludeSource => match tl.next() {
								Some(t1) => if let Token::Path(p) = t1 {
									asm.code.insert(lno, Code::Directive(Dir::Include(p.to_path_buf(), false)));
									break 'toks;
								} else {
									errs.push(format!("<ERR! {}:{}> Include Source directive expected a path, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> Include Source directive expected a path, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							Wrd::IncludeBinary => match tl.next() {
								Some(t1) => if let Token::Path(p) = t1 {
									asm.code.insert(lno, Code::Directive(Dir::Include(p.to_path_buf(), true)));
									break 'toks;
								} else {
									errs.push(format!("<ERR! {}:{}> Include Binary directive expected a path, found {} instead.", p.display(), lno, t1));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> Include Binary directive expected a path, found nothing instead.", p.display(), lno));
									continue 'lines;
								}
							},
							// Identifier (label def, hopefully)
							Wrd::Identifier(id) => match tl.next() {
								Some(t1) => if let Token::Colon = t1 {
									asm.labels.insert(lno, Label { id: id.clone(), local: id.ends_with("$") });
								} else {
									errs.push(format!("<ERR! {}:{}> Expected instruction, directive, or label at start of line, found {} instead.", p.display(), lno, w));
									continue 'lines;
								},
								None => {
									errs.push(format!("<ERR! {}:{}> Expected instruction, directive, or label at start of line, found {} instead.", p.display(), lno, w));
									continue 'lines;
								}
							}
							// Registers (error!)
							_ => {
								errs.push(format!("<ERR! {}:{}> Expected instruction, directive, or label at start of line, found {} instead.", p.display(), lno, w));
								continue 'lines;
							}
						}
					} else {
						errs.push(format!("<ERR! {}:{}> Expected instruction, directive, or label at start of line, found {} instead.", p.display(), lno, t0));
						continue 'lines;
					},
					None => break 'toks
				}
			}

			if tl.len() != 0 { wrns.push(format!("[WARN {}:{}] Only one instruction or directive per line is supported.", p.display(), lno)); }
		}

		if !errs.is_empty() { return Err((errs, wrns)) }
		
		// now we do checks related to identifiers
		// symbols are guaranteed to be unique, but labels are not at this time. so let's make sure all global labels are unique first.
		let mut seen_global_labels: HashMap<String, usize> = HashMap::new();
		for (lno, label) in asm.labels.iter().filter(|(_, l)| !l.local) {
			if !seen_global_labels.contains_key(&label.id) {
				// alright, record the label and line number
				seen_global_labels.insert(label.id.clone(), *lno);
			} else {
				errs.push(format!("<ERR! {}:{}> Global label '{}' is already defined on line {}.", p.display(), lno, label.id, seen_global_labels.get(&label.id).unwrap()));
			}
		}

		if !errs.is_empty() { return Err((errs, wrns)) }

		// now we do our local labels
		let mut last_global_label: Option<String> = None;
		let mut seen_local_labels: HashMap<String, usize> = HashMap::new();
		for (lno, label) in asm.labels.iter() {
			if !label.local {
				last_global_label = Some(label.id.clone());
				seen_local_labels.clear();
			} else if let None = last_global_label {
				errs.push(format!("<ERR! {}:{}> Local labels must be under a global label!", p.display(), lno))
			} else {
				if !seen_local_labels.contains_key(&label.id) {
					// alright, record the label and line number
					seen_local_labels.insert(label.id.clone(), *lno);
				} else {
					errs.push(format!("<ERR! {}:{}> Local label '{}' (under global label '{}') is already defined on line {}.", p.display(), lno, label.id, last_global_label.clone().unwrap(), seen_local_labels.get(&label.id).unwrap()));
				}
			}
		}

		if !errs.is_empty() { return Err((errs, wrns)) }

		// TODO: figure out how to perform identifier argument checking

		if errs.is_empty() {
			Ok((asm, wrns))
		} else {
			Err((errs, wrns))
		}
	}
}

impl ProgAsm {
	pub fn link(fs: HashMap<&Path, FileAsm>) -> ProgAsmResult {
		let mut asm = ProgAsm { symbols: HashMap::new(), labels: HashMap::new(), code: HashMap::new() };
		let mut errs = Vec::new();
		let mut wrns = Vec::new();

		// ...

		if errs.is_empty() {
			Ok((asm, wrns))
		} else {
			Err((errs, wrns))
		}
	}

	pub fn to_bytes(&self) -> &[u8] {
		let mut bytes: &[u8] = &[];

		// ...

		bytes
	}
}