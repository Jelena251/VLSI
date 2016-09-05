library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_signed.all;

use work.data_types.all;

entity execute is
	port(
		clk : in std_logic;
		rst : in std_logic;
		
		data_in : in id_out_signal;
		data_out : out ex_out_signal;
		flush_from_mem : in std_logic;
		data_out_fwd : out forwd_in_signal;
		data_pred_out : out pred_ex_in_signal
	);
end entity execute;

architecture exect of execute is
	signal dat_curr, dat_next : ex_in_signal;
	signal result : word_def;
	signal ret_addr : if_in_signal;
	
begin

	process (clk,rst) is
	begin
		if(rst = '1') then
			dat_curr.is_full <= '0';
		elsif(rising_edge(clk)) then
			dat_curr <= dat_next;
		end if;
	end process;
	
	dat_next.pc_curr <= data_in.pc_curr;
	dat_next.npc <= data_in.npc;
	dat_next.op <= data_in.op;
	dat_next.dest_reg <= data_in.dest_reg;
	dat_next.reg_a <= data_in.reg_a;
	dat_next.reg_b <= data_in.reg_b;
	dat_next.a <= data_in.a;
	dat_next.b <= data_in.b;
	dat_next.d <= data_in.d;
	dat_next.extnd_imm <= data_in.extnd_imm;
	dat_next.is_full <= data_in.is_full when flush_from_mem = '0' else '0';
		
	process (dat_curr) is
	begin
		ret_addr.cond <= '0';
		ret_addr.jump_adr <= (others => '0');
		result <= (others => '0');
	
		case dat_curr.op is
			when ADD => result <= dat_curr.a + dat_curr.b; 
			when ADDI => result <= dat_curr.a + dat_curr.extnd_imm;
			when SUB => result <= dat_curr.a - dat_curr.b; 
			when SUBI => result <= dat_curr.a - dat_curr.extnd_imm;
			
			when MOV => result <= dat_curr.a;
			when MOVI => result(15 downto 0) <= dat_curr.extnd_imm(15 downto 0); 
							 result(31 downto 16) <= dat_curr.d(31 downto 16); 
			
			when AND_A => result <= dat_curr.a and dat_curr.b;
			when OR_A => result <= dat_curr.a or dat_curr.b;
			when XOR_A => result <= dat_curr.a xor dat_curr.b;
			when NOT_A => result <= not dat_curr.a;
			when SHL => result <= to_stdlogicvector(to_bitvector(dat_curr.d) sll to_integer(signed(dat_curr.extnd_imm)));
			when SHR => result <= to_stdlogicvector(to_bitvector(dat_curr.d) srl to_integer(signed(dat_curr.extnd_imm)));
			when SAR => result <= to_stdlogicvector(to_bitvector(dat_curr.d) sra to_integer(signed(dat_curr.extnd_imm)));
			when ROL_A => result <= to_stdlogicvector(to_bitvector(dat_curr.d) rol to_integer(signed(dat_curr.extnd_imm)));
			when ROR_A => result <= to_stdlogicvector(to_bitvector(dat_curr.d) ror to_integer(signed(dat_curr.extnd_imm)));
			
			when BEQ =>
				result <= dat_curr.npc + dat_curr.extnd_imm;
				if(dat_curr.a = dat_curr.b) then
					ret_addr.cond <= '1';
				end if;
			when BNQ =>
				result <= dat_curr.npc + dat_curr.extnd_imm;
				if(dat_curr.a /= dat_curr.b) then
					ret_addr.cond <= '1';
				end if;
			when BLT =>
				result <= dat_curr.npc + dat_curr.extnd_imm;
				if(dat_curr.a < dat_curr.b) then
					ret_addr.cond <= '1';
				end if;
			when BGT =>
				result <= dat_curr.npc + dat_curr.extnd_imm;
				if(dat_curr.a > dat_curr.b) then
					ret_addr.cond <= '1';
				end if;
			when BLE =>
				result <= dat_curr.npc + dat_curr.extnd_imm;
				if(dat_curr.a <= dat_curr.b) then
					ret_addr.cond <= '1';
				end if;
			when BGE =>
				result <= dat_curr.npc + dat_curr.extnd_imm;
				if(dat_curr.a >= dat_curr.b) then
					ret_addr.cond <= '1';
				end if;

			when JSR | JMP =>
				result <= dat_curr.a + dat_curr.extnd_imm;

			when others =>
				result <= (others => '0');
		end case;
		
	end process;
	
	data_out.in_sig <= dat_curr;
	data_out.alu_out <= result;
	
	data_pred_out.instruction <= dat_curr.pc_curr;
	data_pred_out.cond <= true when ret_addr.cond = '1' else
								 false;
	data_pred_out.destination <= result when ret_addr.cond = '1' else
										  dat_curr.pc_curr + 1;
	data_pred_out.branch_full <= true when dat_curr.is_full = '1' and
											(dat_curr.op = BEQ or dat_curr.op = BNQ or dat_curr.op = BLT or dat_curr.op = BGT or dat_curr.op = BLE or dat_curr.op = BGE) else
										  false;
	
	data_out_fwd.valid <= '1' when dat_curr.is_full = '1' and (dat_curr.op = MOV or dat_curr.op = MOVI
					or dat_curr.op = ADD or dat_curr.op = SUB or dat_curr.op = ADDI or dat_curr.op = SUBI
					or dat_curr.op = AND_A or dat_curr.op = OR_A or dat_curr.op = XOR_A or dat_curr.op = NOT_A
					or dat_curr.op = SHL or dat_curr.op = SHR or dat_curr.op = SAR or dat_curr.op = ROR_A
					or dat_curr.op = ROL_A or dat_curr.op = POP or dat_curr.op = LOAD) else
								'0';
	data_out_fwd.op <= dat_curr.op;
	data_out_fwd.dest_reg <= dat_curr.dest_reg;
	data_out_fwd.d <= result; 
	
 	
end architecture exect;

	