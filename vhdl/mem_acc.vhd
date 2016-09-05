library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_signed.all;

use work.data_types.all;

entity memory is
	port(
		clk : in std_logic;
		rst : in std_logic;
		
		flush_from_mem : out std_logic;
		pc_mem : out word_def;
		data_in : in ex_out_signal;
		data_out : out mem_out_signal;
		data_mem_out : out data_mem_signal;
		data_mem_in : in word_def;
		data_out_fwd : out forwd_in_signal;

		is_halt_out : out std_logic
	);
end entity memory;

architecture mem_access of memory is
	signal dat_curr, dat_next : mem_in_signal;
	signal sp_curr, sp_next : word_def;
	signal dat_mem : data_mem_signal;
	signal mem_flush : std_logic;
begin
	
	process(clk, rst)
	begin
		if (rst = '1')
		then
			sp_curr <= std_logic_vector(to_unsigned(2**MEM_LENGTH - 5, 32));
			dat_curr.in_sig.is_full <= '0';
		elsif (rising_edge(clk))
		then
			sp_curr <= sp_next;
			dat_curr <= dat_next;
			
		end if;
	end process;
	
	process(data_in, mem_flush)
	begin
		dat_next <= data_in;
		dat_next.in_sig.is_full <= (not mem_flush) and data_in.in_sig.is_full;
	end process;
	
	process(dat_curr, sp_curr, data_mem_in)
	begin
		dat_mem.adr <= (others => '0');
		dat_mem.v <= (others => 'X');
		dat_mem.wr <= '0';
		sp_next <= sp_curr;
		mem_flush <= '0';
		pc_mem <= (others => '0');
		
		case dat_curr.in_sig.op is
			when LOAD =>
				dat_mem.adr <= dat_curr.in_sig.a + dat_curr.in_sig.extnd_imm;
			when STORE =>
				dat_mem.wr <= dat_curr.in_sig.is_full;
				dat_mem.v <= dat_curr.in_sig.b;
				dat_mem.adr <= dat_curr.in_sig.a + dat_curr.in_sig.extnd_imm;
			when PUSH =>
				dat_mem.wr <= dat_curr.in_sig.is_full;
				dat_mem.v <= dat_curr.in_sig.a;
				dat_mem.adr <= sp_curr;
				if(dat_curr.in_sig.is_full = '1') then
					sp_next <= sp_curr - 1;
				end if;
			when POP =>
				dat_mem.wr <= dat_curr.in_sig.is_full;
				dat_mem.adr <= sp_curr + 1;
				if(dat_curr.in_sig.is_full = '1') then
				sp_next <= sp_curr + 1;
				end if;
			when JMP =>
				pc_mem <= dat_curr.alu_out;
				mem_flush <= dat_curr.in_sig.is_full;
			when JSR =>
				dat_mem.wr <= dat_curr.in_sig.is_full;
				dat_mem.v <= dat_curr.in_sig.pc_curr + 1;
				dat_mem.adr <= sp_curr;
				sp_next <= sp_curr - 1;
				pc_mem <= dat_curr.alu_out;
				mem_flush <= dat_curr.in_sig.is_full;
			when RTS =>
				dat_mem.wr <= dat_curr.in_sig.is_full;
				dat_mem.adr <= sp_curr + 1;
				if(dat_curr.in_sig.is_full = '1') then
					sp_next <= sp_curr + 1;
				end if;
				mem_flush <= dat_curr.in_sig.is_full;
				pc_mem <= data_mem_in;
			when HALT =>
				mem_flush <= dat_curr.in_sig.is_full;
				pc_mem <= dat_curr.in_sig.pc_curr;
			when others =>
				--data_out.result <= dat_curr.alu_out; --(others => '0');
		end case;
		
	end process;
	
	data_out.dest_reg <= dat_curr.in_sig.dest_reg;
	data_out.op <= dat_curr.in_sig.op;
	data_out.is_full <= dat_curr.in_sig.is_full;
	data_mem_out <= dat_mem;
	
	flush_from_mem <= mem_flush;
	
	data_out.result <= data_mem_in when dat_curr.in_sig.op = LOAD or  dat_curr.in_sig.op = POP
							else dat_curr.alu_out;
							
	data_out_fwd.op <= dat_curr.in_sig.op;
	data_out_fwd.dest_reg <= dat_curr.in_sig.dest_reg;
	data_out_fwd.valid <= '1' when dat_curr.in_sig.is_full = '1' and (dat_curr.in_sig.op = MOV or dat_curr.in_sig.op = MOVI
					or dat_curr.in_sig.op = ADD or dat_curr.in_sig.op = SUB or dat_curr.in_sig.op = ADDI or dat_curr.in_sig.op = SUBI
					or dat_curr.in_sig.op = AND_A or dat_curr.in_sig.op = OR_A or dat_curr.in_sig.op = XOR_A or dat_curr.in_sig.op = NOT_A
					or dat_curr.in_sig.op = SHL or dat_curr.in_sig.op = SHR or dat_curr.in_sig.op = SAR or dat_curr.in_sig.op = ROR_A
					or dat_curr.in_sig.op = ROL_A or dat_curr.in_sig.op = POP or dat_curr.in_sig.op = LOAD) else
								'0';
	data_out_fwd.d <= data_mem_in when dat_curr.in_sig.op = LOAD or dat_curr.in_sig.op = POP 
							 else dat_curr.alu_out;
	
	is_halt_out <= '1' when dat_curr.in_sig.op = HALT and dat_curr.in_sig.is_full = '1' else
				   '0';
end architecture mem_access;