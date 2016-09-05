library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use ieee.numeric_std.all;

use work.data_types.all;

entity data_cache is
	port(
		clk : in std_logic;
		rst : in std_logic;
		
		data_in : in data_mem_signal;
		data_out : out word_def
	);
end entity data_cache;

architecture rtl of data_cache is
	type data_memory is array (0 to 2**MEM_LENGTH - 1) of word_def;
	
	signal data : data_memory;
	
	file load_file : text open read_mode is "javni_test_data_in.txt";
	
begin
	process(clk, rst)
	 	variable position : address_def;
		variable dat : word_def;
		variable ln : line;
	begin
		if (rst = '1')
		then	
			while not endfile(load_file) loop
				readline(load_file, ln);
				hread(ln, position);
				read(ln, dat);

				data(to_integer(unsigned(position))) <= dat;
			end loop;
		
		elsif (rising_edge(clk))
		then
			if (data_in.wr = '1')
			then
				data(to_integer(unsigned(data_in.adr))) <= data_in.v;
			end if;
		end if;
	end process;
	
	data_out <= data(to_integer(unsigned(data_in.adr)));
end architecture rtl;