library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_signed.all;

use work.data_types.all;

entity instruction_fetch is 
	port(
		clk : in std_logic;
		rst : in std_logic;
		stall : in std_logic;
		
		pc_start : in address_def;
		data_out : out if_out_signal;
		data_inst_out : out instr_mem_signal;
		
		flush_from_mem : in std_logic;
		pc_mem : in word_def;
		data_pred_out : out address_def;
		data_pred_in : in pred_if_out_signal;

		is_halt_in : in std_logic
	);
end entity instruction_fetch;

architecture inst_fetch of instruction_fetch is
	signal pc_next : address_def;
	signal pc_curr : address_def;
	signal pc_prev : address_def;

	signal is_halt : std_logic;
begin	
	
	process (clk, rst, pc_start) is
	begin
		if (rst = '1') then
			pc_curr <= pc_start;
			is_halt <= '0';
		elsif (rising_edge(clk)) then
			pc_curr <= pc_next;
			pc_prev <= pc_curr;

			if (is_halt = '0')
			then
				is_halt <= is_halt_in;
			end if;
		end if;
	end process;
	
	pc_next <= pc_curr when is_halt = '1' else
				  pc_mem when flush_from_mem = '1' else
				  data_pred_in.flush_destination when data_pred_in.flush = '1' else
				  pc_curr when stall = '1' else
				  data_pred_in.destination when data_pred_in.is_hit = '1' else
				  pc_curr + 1;
	
	data_inst_out.adr <= pc_curr when stall = '0' else pc_prev;
	data_out.pc_curr <= pc_curr;
	data_out.npc <= pc_next;
	data_out.is_full <= '1' when  flush_from_mem = '0' and data_pred_in.flush = '0' and is_halt = '0' else
						'0';
	data_out.prediction <= data_pred_in.prediction;
	data_out.flush <= data_pred_in.flush;
	
	data_pred_out <= pc_curr;
	
end inst_fetch;
	