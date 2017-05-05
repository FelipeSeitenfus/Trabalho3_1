-------------------------------------------------------------------------
-- Design unit: R8_uC
-- Description: 
-------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;        

entity R8_uC is
    port (
        clk: in std_logic;
        rst: in std_logic;
        port_A: inout std_logic_vector(15 downto 0);
        port_B: inout std_logic_vector(15 downto 0)
        );
end R8_uC;

architecture behavioral of R8_uC is
    
    signal dataToR8, dataFromR8 : std_logic_vector(15 downto 0);
    signal addressR8 : std_logic_vector(15 downto 0);
    signal addressMemory : std_logic_vector(14 downto 0);
    signal addressPortA, addressPortB : std_logic_vector(1 downto 0);
    signal dataFromPortA, dataFromPortB : std_logic_vector (15 downto 0);
    signal rw, ce, wr, nclk, we_n, oe_n : std_logic;
    signal enableMemory, enablePortA, enablePortB : std_logic;
    signal dataBus : std_logic_vector(15 downto 0);
    
    
begin  
    PROCESSOR: entity work.R8                -- processor
    port map (
        clk         => clk, 
        rst         => rst, 
        data_in     => dataToR8,          -- data from processor
        data_out    => dataFromR8,           -- data to processor
        address     => addressR8, 
        ce          => ce,               -- memory control
        rw          => rw          --
        );                     
    
    RAM : entity work.Memory   
    generic map (
        SIZE         => 1024,    -- 1024 words (2KB)
        --imageFileName => "Todas_Instrucoes_R8.txt"
        imageFileName => "crypto.txt"
        
        )
    port map (
        clk     => clk,
        ce_n    => enableMemory, 
        we_n    => we_n, 
        oe_n    => oe_n, 
        data    => dataBus, 
        address => addressR8
        );
    
    PORTA: entity work.BidirectionalPort
    generic map(
        DATA_WIDTH          => 16,    						-- Port width in bits
        PORT_DATA_ADDR      => "00",    
        PORT_CONFIG_ADDR    => "01",     
        PORT_ENABLE_ADDR    => "10"     
        )
    port map(
        clk => clk, -- nclk?
        rst => rst,
        
        data_i => dataFromR8,
        data_o => dataFromPortA,
        address => addressPortA,
        rw => wr,
        ce => enablePortA,
        
        port_io => port_A   
        );          
    
    PORTB: entity work.BidirectionalPort
    generic map(
        DATA_WIDTH          => 16,    						-- Port width in bits
        PORT_DATA_ADDR      => "00",    
        PORT_CONFIG_ADDR    => "01",     
        PORT_ENABLE_ADDR    => "10"     
        )
    port map(
        clk => clk, 
        rst => rst,
        
        data_i => dataFromR8,
        data_o => dataFromPortB,
        address => addressPortB,
        rw => wr,
        ce => enablePortB,
        
        port_io => port_B   
        );  
    
    dataToR8 <= dataBus when enableMemory = '0' else
				dataFromPortA when enablePortA = '1' else
				dataFromPortB;	
    
    -- PortA control signals
    addressPortA <= addressR8(1 downto 0);
    enablePortA <= '1' when (ce = '1' and addressR8(15 downto 12) = "1000") else '0';
	-- PortB control signals
    addressPortB <= addressR8(1 downto 0);
    enablePortB <= '1' when (ce = '1' and addressR8(15 downto 12) = "1001") else '0';
    -- Memory signals
    oe_n <= '0' when (enableMemory = '0' and rw = '1') else '1';
    we_n <= '0' when (enableMemory = '0' and rw = '0') else '1';
    nclk <= not clk;
    addressMemory <= addressR8(14 downto 0);
    wr <= not rw;
    enableMemory <= '0' when (ce = '1' and addressR8(15) = '0') else '1';
    dataBus <= dataFromR8 when (enableMemory = '0' and rw = '0') else (others => 'Z'); 
    
end behavioral;