library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_signed.all;

use work.data_types.all;
use work.Components.all;

entity cpu is
	port (
		clk : in std_logic;
		rst : in std_logic;
		
		pc_start : in address_def;
		data_instr_in : in word_def;
		data_instr_out : out instr_mem_signal;
		data_mem_out : out data_mem_signal;
		data_mem_in : in word_def
	);
end entity cpu;

architecture rtl of cpu is
	signal if_id : if_out_signal;
	signal id_ex : id_out_signal;
	signal ex_mem : ex_out_signal;
	signal mem_wb : mem_out_signal;
	signal wb_id : wb_out_signal;
	signal addr_in_if : address_def;
	signal data_in_ex : pred_ex_in_signal;
	signal data_out_if : pred_if_out_signal;
	signal pc_mem : word_def;
	signal flush_from_mem : std_logic;
	signal stall : std_logic;
	signal data_out_fwd_id : forwd_id_in_signal;
	signal data_out_fwd_ex : forwd_in_signal;
	signal data_out_fwd_mem : forwd_in_signal;
	signal data_in_fwd : forwd_out_signal;

	signal is_halt : std_logic;

begin

	isf : instruction_fetch
		port map(
			clk => clk,
			rst => rst,
			
			pc_start => pc_start,
			data_out => if_id,
			data_inst_out => data_instr_out,
			pc_mem => pc_mem,
			flush_from_mem => flush_from_mem,
			stall => stall,
			data_pred_out => addr_in_if,
			data_pred_in => data_out_if,

			is_halt_in => is_halt
		);
		
		
	id : instruction_decode
		port map(
			clk => clk,
			rst => rst,
			
			flush_from_mem => flush_from_mem,
			flush_from_pred => data_out_if.flush,
			stall => stall,
			data_in => if_id,
			data_out => id_ex,
			data_instr_in => data_instr_in,
			data_in_fwd => data_in_fwd,
			data_out_fwd => data_out_fwd_id,
			data_in_wb => wb_id
		);
		
	
	ex : execute
		port map(
			clk => clk,
			rst => rst,
			
			flush_from_mem => flush_from_mem,
			data_in => id_ex,
			data_out => ex_mem,
			data_out_fwd => data_out_fwd_ex,
			data_pred_out => data_in_ex
		);
		
		
	mem : memory
		port map(
			clk => clk,
			rst => rst,
			
			pc_mem => pc_mem,
			flush_from_mem => flush_from_mem,
			data_out_fwd => data_out_fwd_mem,
			data_in => ex_mem,
			data_out => mem_wb,
			data_mem_out => data_mem_out,
			data_mem_in => data_mem_in,

			is_halt_out => is_halt
		);
		
		
	wb : write_back
		port map(
			clk => clk,
			rst => rst,
			
			data_in => mem_wb,
			data_out => wb_id
		);
		
	pd : prediction_unit
		port map
		(
			addr_in_if => addr_in_if,
			data_in_ex => data_in_ex,
			data_out_if => data_out_if,
			clk => clk,
			rst => rst
		);
		
	fd : forward_unit
		port map
		(
			data_ex_in => data_out_fwd_ex,
			data_mem_in => data_out_fwd_mem,
			data_id_in => data_out_fwd_id,
			data_out => data_in_fwd
		);

end architecture rtl;