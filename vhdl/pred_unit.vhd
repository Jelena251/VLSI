library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_signed.all;

use work.data_types.all;	

entity prediction_unit is

	port
	(
		-- Input ports
		addr_in_if : in address_def;
		data_in_ex : in pred_ex_in_signal;
		-- Output ports
		data_out_if : out pred_if_out_signal;
		
		clk, rst: in std_logic
	);
end prediction_unit;

architecture prediction_arch of prediction_unit is
	signal cnt_next, cnt_curr : integer := 0;
	signal cache_curr, cache_next : cache_type;
begin
	process(clk, rst)
	begin
		if(rst = '1')
		then
			for i in 0 to CACHE_SIZE - 1
			loop
				cache_curr(i).v <= '0'; 
			end loop;
			cnt_curr <= 0;
		elsif (rising_edge(clk))
		then
			for i in 0 to CACHE_SIZE - 1
			loop
				cache_curr(i) <= cache_next(i);
			end loop;
			cnt_curr <= cnt_next;
		end if;
	end process;
	
	process(addr_in_if, cache_curr, data_in_ex, cnt_curr)
		variable instrHit : boolean;
	begin
		instrHit := false;
		data_out_if.prediction <= TAKEN;
		data_out_if.destination <= (others => '0');
		data_out_if.is_hit <= '0';
		cnt_next <= cnt_curr;
		data_out_if.flush <= '0';
		data_out_if.flush_destination <= (others => '0');
		for i in 0 to CACHE_SIZE - 1
		loop
			cache_next(i) <= cache_curr(i);
		end loop;
		for i in 0 to CACHE_SIZE - 1
		loop
			if(cache_curr(i).instruction = addr_in_if)
			then
				data_out_if.is_hit <= '1';
				data_out_if.prediction <= cache_curr(i).prediction;
				data_out_if.destination <= cache_curr(i).destination;
				
			end if;
		end loop;
		
		--writing from EXE phase
		
		if(data_in_ex.branch_full = true)
		then
			for i in 0 to CACHE_SIZE - 1
			loop
				if(cache_curr(i).v = '1' and cache_curr(i).instruction = data_in_ex.instruction)
				then	
					instrHit := true;
					
					--pocetak
					
					if (cache_curr(i).prediction = TAKEN)
					then
						if (data_in_ex.cond = true)
						then
							cache_next(i).prediction <= STRONG_TAKEN;
						else
							cache_next(i).prediction <= STRONG_NOT_TAKEN;
						end if;
					end if;

					if (cache_curr(i).prediction = STRONG_TAKEN)
					then
						if (data_in_ex.cond = true)
						then
							cache_next(i).prediction <= STRONG_TAKEN;
						else
							cache_next(i).prediction <= TAKEN;
						end if;
					end if;
					
					if (cache_curr(i).prediction = NOT_TAKEN)
					then
						if (data_in_ex.cond = true)
						then
							cache_next(i).prediction <= STRONG_TAKEN;
						else
							cache_next(i).prediction <= STRONG_NOT_TAKEN;
						end if;
					end if;
						
					if (cache_curr(i).prediction = STRONG_NOT_TAKEN)
					then
						if (data_in_ex.cond = true)
						then
							cache_next(i).prediction <= NOT_TAKEN;
						else
							cache_next(i).prediction <= STRONG_NOT_TAKEN;
						end if;
					end if;

					--kraj
					
					if(data_in_ex.cond = true and cache_curr(i).prediction = NOT_TAKEN)
					then 
						cache_next(i).destination <= data_in_ex.destination;
					end if;
					if(data_in_ex.cond = true and (cache_curr(i).prediction = NOT_TAKEN or cache_curr(i).prediction = STRONG_NOT_TAKEN))
					then 
						data_out_if.flush <= '1';
						data_out_if.flush_destination <= data_in_ex.destination;
					end if;
					if(data_in_ex.cond = false and cache_curr(i).prediction = TAKEN)
					then 
						cache_next(i).destination <= data_in_ex.instruction + 1;
					end if;
					if(data_in_ex.cond = false and (cache_curr(i).prediction = TAKEN or cache_curr(i).prediction = STRONG_TAKEN)) 
					then 
						data_out_if.flush <= '1';
						data_out_if.flush_destination <= data_in_ex.instruction + 1;
					end if;
				end if;
			end loop;
		--end if;
		
		if(instrHit = false)
		then
			if(cnt_curr < CACHE_SIZE - 1)
			then
				cache_next(cnt_curr).instruction <= data_in_ex.instruction;
				cache_next(cnt_curr).destination <= data_in_ex.destination;
				if(data_in_ex.cond = true)
				then
					cache_next(cnt_curr).prediction <= TAKEN;
				else
					cache_next(cnt_curr).prediction <= NOT_TAKEN;
				end if;
				cache_next(cnt_curr).v <= '1';
				cnt_next <= cnt_curr + 1;
			else
				cnt_next <= 0;
				cache_next(cnt_curr).instruction <= data_in_ex.instruction;
				cache_next(cnt_curr).destination <= data_in_ex.destination;
				if(data_in_ex.cond = true)
				then
					cache_next(cnt_curr).prediction <= TAKEN;
				else
					cache_next(cnt_curr).prediction <= NOT_TAKEN;
				end if;
			end if;
			
			if(data_in_ex.cond = true)
			then
				data_out_if.flush <= '1';
				data_out_if.flush_destination <= data_in_ex.destination;
			end if;
		end if;
		end if;
					
	end process;
end architecture;
