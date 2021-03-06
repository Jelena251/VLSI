library ieee;
library work;

use ieee.std_logic_1164.all;
use work.data_types.all;
use work.Components.all;

entity test_bench is
end entity;

architecture test_bench_arch of test_bench is
	signal clk, rst : std_logic;
	signal pc_start : address_def;
	signal data_instr_in : word_def;
	signal data_instr_out : instr_mem_signal;
	signal data_mem_out : data_mem_signal;
	signal data_mem_in : word_def;

begin

	procs : cpu
		port map
		(
		clk => clk,
		rst => rst,
		
		pc_start => pc_start,
		data_instr_in => data_instr_in,
		data_instr_out => data_instr_out,
		data_mem_out => data_mem_out,
		data_mem_in => data_mem_in
		);

	InstrMem : instr_cache
		port map
		(
			addr => data_instr_out,
			data_out =>  data_instr_in,
			pc_start => pc_start,
			clk => clk,
			rst => rst
		);
		
	DataMem : data_cache 
		port map
		(
			data_in => data_mem_out,
			data_out => data_mem_in,

			clk => clk,
			rst => rst
		);
		

	process
	begin
		clk <= '1';
		wait for 5 ns;
		clk <= '0';
		wait for 5 ns;
	end process;

	process
	begin
		rst <= '1';
		wait for 15 ns;
		rst <= '0';
		wait;
	end process;
end architecture;