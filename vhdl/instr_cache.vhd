library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use ieee.numeric_std.all;

use work.data_types.all;

entity instr_cache is
	port (
		clk : in std_logic;
		rst : in std_logic;
		
		addr : in instr_mem_signal;
		data_out : out word_def; 
		pc_start : out word_def
	);
end entity instr_cache;

architecture rtl of instr_cache is 
	type instr_memory is array (0 to 2**MEM_LENGTH - 1) of word_def;
	
	signal instr : instr_memory;
	
	file load_file : text open read_mode is "javni_test_inst_in.txt";
begin

	process(rst, clk) 
		variable ln : line;
		variable start_pc : address_def;
		variable data : word_def;
		variable position : address_def;
	begin
		if(rst = '1') then
			if (not endfile(load_file))
				then
					readline(load_file, ln);
					hread(ln, start_pc);
					pc_start <= start_pc;
			end if;
			
			while not endfile(load_file) loop
				readline(load_file, ln);
				hread(ln, position);
				read(ln, data);

				instr(to_integer(unsigned(position))) <= data;
			end loop;
		else
			if (rising_edge(clk))
			then
				data_out <= instr(to_integer(unsigned(addr.adr)));
			end if;	
		end if;
	end process;
	
	
end rtl;