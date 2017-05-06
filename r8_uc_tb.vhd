library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity r8_uC_tb is
end r8_uC_tb;

architecture behavioral of r8_uC_tb is
signal clk : std_logic := '0';
signal rst : std_logic;
signal port_A, port_B : std_logic_vector(15 downto 0);      
signal data_crypto_1, data_crypto_2, data_from_crypto_1, data_from_crypto_2: std_logic_vector(7 downto 0);
begin
	R8_uC: entity work.r8_uC
    	port map(
            	clk => clk,
            	rst => rst,
            	port_A => port_A,
		port_B => port_B
        );     
        
	CRYPTO_1: entity work.CryptoMessage
    	generic map (
        	MSG_INTERVAL    => 10,    -- Clock cycles
        	FILE_NAME       => "DoctorRockter.txt"
    	)
    	port map( 
        	clk         => clk,
        	rst         => rst,
        	ack         => port_A(15),
        	data_in     => port_A(7 downto 0),
        	data_out    => data_from_crypto_1,
        	data_av     => port_A(14),
        	keyExchange => port_B(10), --ambos vem de Port_B
        	eom         => port_A(12)
    	);
    
	CRYPTO_2: entity work.CryptoMessage
    	generic map (
        	MSG_INTERVAL    => 10,    -- Clock cycles
        	FILE_NAME       => "RevolutionCalling.txt"
    	)
    	port map( 
        	clk         => clk,
        	rst         => rst,
        	ack         => port_B(15), 
        	data_in     => port_B(7 downto 0),
        	data_out    => data_from_crypto_2,
        	data_av     => port_B(14),
        	keyExchange => port_B(13),
        	eom         => port_B(12)
    	);
        
    	clk <= not clk after 5 ns; -- 100 MHz
    	rst <= '1', '0' after 5 ns;
	
	port_A(7 downto 0) <= data_from_crypto_1 when port_A(11) = '0' else (others => 'Z');
	port_B(7 downto 0) <= data_from_crypto_2 when port_B(11) = '0' else (others => 'Z');
end behavioral;
