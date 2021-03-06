library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.data_types.all;

entity write_back is
	port(
		clk : in std_logic;
		rst : in std_logic;
		
		data_in : in mem_out_signal;
		data_out : out wb_out_signal
	);
end entity write_back;

architecture wrt_bck of write_back is
	signal dat_curr, dat_next : wb_in_signal;
	
begin
	
	process(clk, rst)
	begin
		if (rst = '1')
		then
			dat_curr.is_full <= '0';
		elsif (rising_edge(clk))
		then
			dat_curr <= dat_next;
		end if;
	end process;
	
	dat_next <= data_in;
	
	data_out.wr <= '1' when dat_curr.is_full = '1' and (dat_curr.op = LOAD or dat_curr.op = MOV or dat_curr.op = MOVI
								or	dat_curr.op = ADD or dat_curr.op = SUB or dat_curr.op = ADDI
								or dat_curr.op = SUBI or dat_curr.op = AND_A or dat_curr.op = OR_A
								or dat_curr.op = XOR_A or dat_curr.op = NOT_A or dat_curr.op = SHL
								or dat_curr.op = SHR or dat_curr.op = SAR or dat_curr.op = ROR_A
								or dat_curr.op = ROL_A or dat_curr.op = POP) -- dat_curr.op = LOAD or  dodati
								else
						'0';
	data_out.reg_num <= dat_curr.dest_reg;
	data_out.reg_val <= dat_curr.result;

end architecture wrt_bck;