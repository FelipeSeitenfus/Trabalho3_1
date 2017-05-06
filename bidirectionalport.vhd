library IEEE;
use IEEE.std_logic_1164.all;

entity BidirectionalPort  is
    generic (
        DATA_WIDTH          : integer;    -- Port width in bits
        PORT_DATA_ADDR      : std_logic_vector(1 downto 0);     -- NÃO ALTERAR!
        PORT_CONFIG_ADDR    : std_logic_vector(1 downto 0);     -- NÃO ALTERAR! 
        PORT_ENABLE_ADDR    : std_logic_vector(1 downto 0);      -- NÃO ALTERAR!
	PORT_IRQ_ENABLE_ADDR : std_logic_vector(1 downto 0)
    );
    port (  
        clk         : in std_logic;
        rst         : in std_logic; 
        
        -- Processor interface
        data_i      : in std_logic_vector (DATA_WIDTH-1 downto 0);
        data_o      : out std_logic_vector (DATA_WIDTH-1 downto 0);
        address     : in std_logic_vector (1 downto 0);		-- NÃO ALTERAR
        rw          : in std_logic; -- 0: read; 1: write
        ce          : in std_logic;
        
        -- External interface
        port_io     : inout std_logic_vector (DATA_WIDTH-1 downto 0);
	-- Vetor de interrupção
	irq	    : out std_logic_vector (DATA_WIDTH-1 downto 0)
    );
end BidirectionalPort ;


architecture Behavioral of BidirectionalPort  is
	signal PortData, PortConfig, PortEnable, irqEnable, synch : std_logic_vector(DATA_WIDTH-1 downto 0);
	signal PortData_In, synch_in : std_logic_vector(DATA_WIDTH-1 downto 0);
begin
	process(clk, rst)
	begin
		if rst = '1' then 
			PortData <= (others => '0');
			PortConfig <= (others => '1'); -- input as default
			PortEnable <= (others => '0'); -- disabled
			irqEnable <= (others => '0'); -- disabled
			synch <= (others => '0');
		elsif rising_edge(clk) then
			synch <= synch_in;
			--if(address = PORT_DATA_ADDR and ce = '1') then
				--PortData <= PortData_In;
            for i in 0 to DATA_WIDTH-1 loop
                if((ce = '1' and rw = '1' and address = PORT_DATA_ADDR) or (PortEnable(i) = '1' and PortConfig(i) = '1')) then
                    PortData(i) <= PortData_In(i); 
                end if;
            end loop;
			if (address = PORT_CONFIG_ADDR and ce = '1' and rw = '1') then
				PortConfig <= data_i;
			elsif (address = PORT_ENABLE_ADDR and ce = '1' and rw = '1') then
				PortEnable <= data_i;
			elsif (address = PORT_IRQ_ENABLE_ADDR and ce = '1' and rw = '1') the
				irqEnable <= data_i;
			end if;
		end if;
	end process;
	   
    COMBINATIONAL: for i in 0 to DATA_WIDTH-1 generate
	irq(i) <= PortData(i) and PortEnable(i) and PortConfig(i) and irqEnable(i);
        port_io(i) <= PortData(i) when PortConfig(i) = '0' and PortEnable(i) = '1' else 'Z';
        PortData_In(i) <= synch(i) when PortConfig(i) = '1' and PortEnable(i) = '1' else data_i(i);
        synch_in(i) <= port_io(i) when PortConfig(i) = '1' and PortEnable(i) = '1' else 'Z';
    end generate COMBINATIONAL;
	
	
	data_o <= PortData when address = PORT_DATA_ADDR else
			  PortConfig when address = PORT_CONFIG_ADDR else
			  PortEnable;
end Behavioral;
	
