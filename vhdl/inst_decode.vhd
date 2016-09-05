library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.data_types.all;

entity instruction_decode is
	port(
		clk : in std_logic;
		rst : in std_logic;
		
		stall : out std_logic;
		flush_from_pred : in std_logic;
		flush_from_mem : in std_logic;
		data_in : in if_out_signal;
		data_instr_in : in word_def;
		data_out : out id_out_signal;
		data_out_fwd : out forwd_id_in_signal;
		data_in_fwd : in forwd_out_signal; 
		data_in_wb : in wb_out_signal
	);
end entity instruction_decode;

architecture instr_dec of instruction_decode is
	
	signal regs_curr, regs_next : regs_f; 
	signal dat_curr, dat_next : if_out_signal;
	signal decoded : id_out_signal;
	
begin
	
	process (clk, rst) is
	begin	
		if (rst = '1') then
			for i in 0 to 31 loop
				regs_curr(i) <= (others => '0');--std_logic_vector(to_signed(i, word_def'length));
			end loop;
			dat_curr.is_full <= '0';
		elsif(falling_edge(clk)) then
			for j in 0 to 31 loop
				regs_curr(j) <= regs_next(j);
			end loop;
		elsif (rising_edge(clk)) then
			dat_curr <= dat_next;
		end if;
	end process;
	
	process (data_in_wb, regs_curr) is
	begin
		for j in 0 to 31 loop
			regs_next(j) <= regs_curr(j);
		end loop;
		
		if (data_in_wb.wr = '1') then
			regs_next(data_in_wb.reg_num) <= data_in_wb.reg_val; 
		end if;
	end process;
	
	dat_next <= data_in when data_in_fwd.stall = '0' else
					dat_curr;
	
	decoded <= decod_instr(data_instr_in, regs_curr, dat_curr, data_in_fwd, flush_from_mem, flush_from_pred);
	
	stall <= data_in_fwd.stall;
	
	data_out_fwd.op <= decoded.op;
	data_out_fwd.dest_reg <= decoded.dest_reg;
	data_out_fwd.reg_a <= decoded.reg_a;
	data_out_fwd.reg_b <= decoded.reg_b;
	data_out_fwd.d <= decoded.d;
	data_out_fwd.b <= decoded.b;
	data_out_fwd.a <= decoded.a;
	

	data_out <= decoded;
	
	
end architecture instr_dec;

