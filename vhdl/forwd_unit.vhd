library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_signed.all;

use work.data_types.all;

entity forward_unit is
	port(
		data_ex_in : in forwd_in_signal;
		data_mem_in : in forwd_in_signal;
		data_id_in : in forwd_id_in_signal;
		data_out: out forwd_out_signal
	);
end entity forward_unit;

architecture forward_arch of forward_unit is
	signal dat : forwd_out_signal;
begin
	process(data_ex_in, data_id_in, data_mem_in) is
	begin
		dat.dest_reg_hzd <= '0';
		dat.reg_a_hzd <= '0';
		dat.reg_b_hzd <= '0';
		dat.forwd_value_a <= (others => '0');
		dat.forwd_value_b <= (others => '0');
		dat.forwd_value_d <= (others => '0');
		dat.stall <= '0';
		
		if(data_ex_in.valid = '1' and data_ex_in.dest_reg = data_id_in.reg_a)
		then
			dat.reg_a_hzd <= '1';
			dat.forwd_value_a <= data_ex_in.d;
		elsif(data_mem_in.valid = '1' and data_mem_in.dest_reg = data_id_in.reg_a)
		then
			dat.reg_a_hzd <= '1';
			dat.forwd_value_a <= data_mem_in.d;
		end if;
		
		if(data_ex_in.valid = '1' and data_ex_in.dest_reg = data_id_in.reg_b)
		then
			dat.reg_b_hzd <= '1';
			dat.forwd_value_b <= data_ex_in.d;
		elsif(data_mem_in.valid = '1' and data_mem_in.dest_reg = data_id_in.reg_b)
		then
			dat.reg_b_hzd <= '1';
			dat.forwd_value_b <= data_mem_in.d;
		end if;
		
		if(data_ex_in.valid = '1' and data_ex_in.dest_reg = data_id_in.dest_reg)
		then
			dat.dest_reg_hzd <= '1';
			dat.forwd_value_d <= data_ex_in.d;
		elsif(data_mem_in.valid = '1' and data_mem_in.dest_reg = data_id_in.reg_b)
		then
			dat.dest_reg_hzd <= '1';
			dat.forwd_value_d <= data_mem_in.d;
		end if;
		
		if(data_ex_in.valid = '1' and (data_ex_in.op = LOAD or data_ex_in.op = POP) and 
			((data_id_in.reg_a = data_ex_in.dest_reg and 
			(data_id_in.op = LOAD or data_id_in.op = STORE or data_id_in.op = MOV or 
			data_id_in.op = ADD or data_id_in.op = SUB or data_id_in.op = AND_A or data_id_in.op = OR_A or
			data_id_in.op = NOT_A or data_id_in.op = XOR_A or data_id_in.op = ADDI or data_id_in.op = SUBI or
			data_id_in.op = JSR or data_id_in.op = JMP or data_id_in.op = BLE or data_id_in.op = BEQ or 
			data_id_in.op = BNQ or data_id_in.op = BGT or data_id_in.op = BLT or data_id_in.op = BGE)) or
			(data_id_in.reg_b = data_ex_in.dest_reg and 
			(data_id_in.op = ADD or data_id_in.op = SUB or data_id_in.op = AND_A or data_id_in.op = OR_A or
			data_id_in.op = XOR_A or data_id_in.op = NOT_A or data_id_in.op = BLE or data_id_in.op = BEQ or 
			data_id_in.op = BNQ or data_id_in.op = BGT or data_id_in.op = BLT or data_id_in.op = BGE)) or
			(data_id_in.dest_reg = data_ex_in.dest_reg and 
			(data_id_in.op = ROL_A or data_id_in.op = ROR_A or data_id_in.op = SHR or 
			data_id_in.op = SHL or data_id_in.op = SAR or data_id_in.op = MOVI)))) 
		then
			dat.stall <= '1';
		end if;	
		
	end process;
	
	data_out <= dat;

end architecture;