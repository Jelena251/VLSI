library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package data_types is
constant ADDR_LENGTH : integer := 32;
constant WORD_LENGTH : integer := 32;
constant MEM_LENGTH : integer := 15;
constant REGISTER_NUM : integer := 32;
constant CACHE_SIZE : integer := 20;

subtype address_def is std_logic_vector(ADDR_LENGTH -1  downto 0);

subtype word_def is std_logic_vector(WORD_LENGTH - 1 downto 0);

type predict_def is (STRONG_TAKEN, TAKEN, NOT_TAKEN, STRONG_NOT_TAKEN);

type if_in_signal is record
	jump_adr : address_def;
	cond : std_logic;
end record if_in_signal;

type if_out_signal is record
	npc, pc_curr : address_def;
	is_full : std_logic;
	prediction : predict_def;
	flush : std_logic;
end record if_out_signal;

type instr_mem_signal is record
	adr : address_def;
end record instr_mem_signal;

type instr_data is record
	instr : address_def;
end record instr_data;

type id_in_signal is record
	npc : address_def;	
end record id_in_signal;

type instruction is (LOAD, STORE, MOV, MOVI, ADD, SUB, ADDI, SUBI, AND_A, OR_A, XOR_A, NOT_A, SHL, SHR, SAR, ROL_A, ROR_A, JMP, JSR, RTS, PUSH, POP, BEQ, BNQ, BGT, BLT, BGE, BLE, HALT);

type id_out_signal is record
	npc, pc_curr : address_def;
	op : instruction;
	dest_reg : integer;
	reg_a : integer;
	reg_b : integer;
	a : word_def;
	b : word_def;
	d : word_def;
	extnd_imm : word_def;
	is_full : std_logic;
end record id_out_signal;

subtype ex_in_signal is id_out_signal;

type ex_out_signal is record
	in_sig : ex_in_signal;
	alu_out : word_def;
end record ex_out_signal;

subtype mem_in_signal is ex_out_signal;

type data_mem_signal is record
	adr : address_def;
	v : word_def;
	wr : std_logic;
end record data_mem_signal;

type mem_out_signal is record 
	result :  word_def;
	dest_reg : integer;
	op : instruction;
	is_full : std_logic;
end record mem_out_signal;

subtype wb_in_signal is mem_out_signal;

type wb_out_signal is record 
	reg_num : integer;
	reg_val : word_def;
	wr : std_logic;
end record wb_out_signal;


type regs_f is array (0 to REGISTER_NUM - 1) of word_def;


type cache_elem is record
		instruction : address_def;
		destination : address_def;
		prediction : predict_def;
		v : std_logic;
end record;

type pred_if_out_signal is record
	destination : address_def;
	prediction : predict_def;
	is_hit : std_logic;
	flush : std_logic;
	flush_destination : address_def;
end record;

type pred_ex_in_signal is record
	instruction : address_def;
	destination : address_def;
	cond : boolean;
	branch_full: boolean;
end record;

type forwd_out_signal is record
	dest_reg_hzd : std_logic;
	reg_a_hzd : std_logic;
	reg_b_hzd : std_logic;
	forwd_value_a, forwd_value_b, forwd_value_d : word_def;
	stall : std_logic;
end record;

type forwd_in_signal is record
	dest_reg : integer;
	d : word_def;
	op : instruction;
	valid : std_logic;
end record;

type forwd_id_in_signal is record
	dest_reg : integer;
	reg_a : integer;
	reg_b : integer;
	a : word_def;
	b : word_def;
	d : word_def;
	op : instruction; 
end record;

type cache_type is array (0 to CACHE_SIZE -1) of cache_elem;

function decod_instr(instr : word_def; regs_curr: regs_f; dat_curr: if_out_signal; data_in_fwd: forwd_out_signal; flush_from_mem, flush_from_pred: std_logic) return id_out_signal;

end package data_types;

package body data_types is 

function decod_instr(instr : word_def; regs_curr: regs_f; dat_curr: if_out_signal; data_in_fwd: forwd_out_signal; flush_from_mem, flush_from_pred: std_logic) return id_out_signal is
	variable ret : id_out_signal;
begin
	ret.pc_curr := dat_curr.pc_curr;
	ret.npc := dat_curr.npc; --(others => '0');
	ret.op := HALT;
	ret.dest_reg := 0;
	ret.reg_a := 0;
	ret.reg_b := 0;
	ret.a := (others => '0');
	ret.b := (others => '0');
	ret.d := (others => '0');
	ret.extnd_imm := (others => '0');
	if (dat_curr.flush = '0' and data_in_fwd.stall = '0' and flush_from_mem = '0' and flush_from_pred = '0') then
		ret.is_full := dat_curr.is_full;
	else	
		ret.is_full := '0';
	end if;
	
	case instr(31 downto 26) is
		when "000000" =>
			ret.op := LOAD;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.extnd_imm(15 downto 0) := instr(15 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "000001" =>
			ret.op := STORE;
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
			ret.extnd_imm(15 downto 11) := instr(25 downto 21);
			ret.extnd_imm(10 downto 0) := instr(10 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "000100" =>
			ret.op := MOV;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
		when "000101" =>
			ret.op := MOVI;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.extnd_imm(15 downto 0) := instr(15 downto 0);
		when "001000" =>
			ret.op := ADD;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
		when "001001" =>
			ret.op := SUB;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
		when "001100" =>
			ret.op := ADDI;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.extnd_imm(15 downto 0) := instr(15 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "001101" =>
			ret.op := SUBI;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.extnd_imm(15 downto 0) := instr(15 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "010000" =>
			ret.op := AND_A;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
		when "010001" =>
			ret.op := OR_A;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
		when "010010" =>
			ret.op := XOR_A;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
		when "010011" =>
			ret.op := NOT_A;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
		when "011000" =>
			ret.op := SHL;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.extnd_imm(4 downto 0) := instr(15 downto 11);
		when "011001" =>
			ret.op := SHR;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.extnd_imm(4 downto 0) := instr(15 downto 11);
		when "011010" =>
			ret.op := SAR;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.extnd_imm(4 downto 0) := instr(15 downto 11);
		when "011011" =>
			ret.op := ROL_A;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.extnd_imm(4 downto 0) := instr(15 downto 11);
		when "011100" =>
			ret.op := ROR_A;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.extnd_imm(4 downto 0) := instr(15 downto 11);
		when "100000" =>
			ret.op := JMP;
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.extnd_imm(15 downto 0) := instr(15 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "100001" =>
			ret.op := JSR;
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.extnd_imm(15 downto 0) := instr(15 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "100010" =>
			ret.op := RTS;
		when "100100" =>
			ret.op := PUSH;
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
		when "100101" =>
			ret.op := POP;
			ret.dest_reg := to_integer(unsigned(instr(25 downto 21)));
			ret.d := regs_curr(ret.dest_reg);
		when "101000" =>
			ret.op := BEQ;
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
			ret.extnd_imm(15 downto 11) := instr(25 downto 21);
			ret.extnd_imm(10 downto 0) := instr(10 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "101001" =>
			ret.op := BNQ;
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
			ret.extnd_imm(15 downto 11) := instr(25 downto 21);
			ret.extnd_imm(10 downto 0) := instr(10 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "101011" =>
			ret.op := BLT;
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
			ret.extnd_imm(15 downto 11) := instr(25 downto 21);
			ret.extnd_imm(10 downto 0) := instr(10 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "101010" =>
			ret.op := BGT;
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
			ret.extnd_imm(15 downto 11) := instr(25 downto 21);
			ret.extnd_imm(10 downto 0) := instr(10 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "101101" =>
			ret.op := BLE;
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
			ret.extnd_imm(15 downto 11) := instr(25 downto 21);
			ret.extnd_imm(10 downto 0) := instr(10 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "101100" =>
			ret.op := BGE;
			ret.reg_a := to_integer(unsigned(instr(20 downto 16)));
			ret.a := regs_curr(ret.reg_a);
			ret.reg_b := to_integer(unsigned(instr(15 downto 11)));
			ret.b := regs_curr(ret.reg_b);
			ret.extnd_imm(15 downto 11) := instr(25 downto 21);
			ret.extnd_imm(10 downto 0) := instr(10 downto 0);
			if(ret.extnd_imm(15) = '1') then
				ret.extnd_imm(31 downto 16) := (others => '1');
			end if;
		when "111111" =>
			ret.op := HALT;
		when others =>
			--null;
			ret.op := HALT;
	end case;
	
	if(data_in_fwd.dest_reg_hzd = '1')
	then
		ret.d := data_in_fwd.forwd_value_d;
	end if;
	
	if(data_in_fwd.reg_a_hzd = '1')
	then
		ret.a := data_in_fwd.forwd_value_a;
	end if;
	
	if(data_in_fwd.reg_b_hzd = '1')
	then
		ret.b := data_in_fwd.forwd_value_b;
	end if;
	
	return ret;
end function decod_instr;

end package body;