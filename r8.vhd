-------------------------------------------------------------------------
--
--  R8 PROCESSOR   -  GOLD VERSION  -  05/JAN/2017
--
--  moraes - 30/09/2001  - project start
--  moraes - 22/11/2001  - instruction decoding bug correction
--  moraes - 22/03/2002  - store instruction correction            
--  moraes - 05/04/2003  - SIDLE state inclusion in the control unit
--  calazans - 02/05/2003  - translation of comments to English. Names of
--    some signals, entities, etc have been changed accordingly
--  carara - 03/2013 - project split in several files. Each entity is described in a file with the same name.
--  carara - 5/01/2017 - library std_logic_unsigned replaced by numeric_std
--
--  Notes: 1) In this version, the register bank is designed using 
--    for-generate VHDL construction
--         2) The top-level R8 entity is
--
--      entity R8 is
--            port( clk,rst: in std_logic;
--                  data_in:  in  std_logic_vector(15 downto 0);    -- Data from memory
--                  data_out: out std_logic_vector(15 downto 0);    -- Data to memory
--                  address: out std_logic_vector(15 downto 0);     -- Address to memory
--                  ce,rw: out std_logic );                         -- Memory control
--      end R8;
-- 
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- Design unit: R8
-- Description: Top-level behavioral description of R8 processor
-------------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;   
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
--use work.R8_pkg.all; 


entity R8 is
	generic(
        INTERRUPT_ADDRESS: std_logic_vector(15 downto 0)
	);
    port( 
        clk     : in std_logic;
        rst     : in std_logic;
        
        -- Memory interface
        data_in : in std_logic_vector(15 downto 0);
        data_out: out std_logic_vector(15 downto 0);
        address : out std_logic_vector(15 downto 0);
	intr	: in std_logic;
        ce      : out std_logic;
        rw      : out std_logic 
    );
end R8;

architecture behavioral of R8 is   

    type InstructionType is (Format1, Format2, other);
	signal instType : InstructionType; -- Tipo da instrução
	type State  is (Sidle, Sfetch, Sreg, Shalt, Salu, Srts, Spop, Sldsp, 
			Sld, Sst, Swbk, Sjmp, Ssbrt, Spush, Smul, Sdiv, Smfh, Smfl,
		        SpushF, SpopF, Srti, Sinterrupt); 
    signal currentState: State;
	type Instruction is ( 
        ADD, SUB, AAND, OOR, XXOR, ADDI, SUBI, NOT_A, 
        SL0, SL1, SR0, SR1,
        LDL, LDH, LD, ST, LDSP, POP, PUSH,
        JUMP_R, JUMP_A, JUMP_D, JSRR, JSR, JSRD,
        NOP, HALT,  RTS, MUL, DIV, MFH, MFL, PUSHF, POPF, RTI
    );
	
    signal flags: std_logic_vector(3 downto 0); -- registrador que guarda as flags da ULA
	signal decodedInstruction : instruction;
	signal PC: std_logic_vector(15 downto 0); -- Program Counter
	signal IR: std_logic_vector(15 downto 0); -- Instruction Register
	signal SP: std_logic_vector(15 downto 0); -- Stack Pointer
	signal RA: std_logic_vector(15 downto 0); -- Registrador que guarda o primeiro registrador lido do registerFile
	signal RB: std_logic_vector(15 downto 0); -- Registrador que guarda o segundo registrador lido do registerFile
	signal RULA: std_logic_vector(15 downto 0); -- Registrador que guarda o valor da ULA
	signal S1: std_logic_vector(15 downto 0); -- primeira saída do registerFile
	signal S2: std_logic_vector(15 downto 0); -- segunda saída do registerFile
	signal adder: std_logic_vector(16 downto 0); -- somador para realizar as operações de soma e subtração
	signal outula: std_logic_vector(15 downto 0); -- saida da ULA
    signal opA, opB: std_logic_vector(15 downto 0); -- operadores da ULA
	signal op1_adder, op2_adder: std_logic_vector(15 downto 0); -- operadores do somador
	signal dtReg: std_logic_vector(15 downto 0);
	signal n, z, c, v :std_logic; -- flags bits 
    signal multiply : std_logic_vector(31 downto 0);
    signal division : std_logic_vector(31 downto 0);
    signal high, low: std_logic_vector(15 downto 0);
    
	-- Register file
    type RegisterArray is array (natural range <>) of std_logic_vector(15 downto 0);
    signal registerFile: RegisterArray(0 to 15);
	
	-- flags
	alias negativeflag  : std_logic is flags(0);
    alias zeroflag      : std_logic is flags(1);
    alias carryflag     : std_logic is flags(2);
    alias overflowflag  : std_logic is flags(3);  
    
    -- Interrrupt signals
    signal interruptReg: std_logic;
    signal atendendo_interrupcao: boolean;
		
begin   
	instType <= Format1 when decodedInstruction=ADD or decodedInstruction=SUB or decodedInstruction=AAND or decodedInstruction=OOR or decodedInstruction=XXOR or decodedInstruction=NOT_A or decodedInstruction=SL0 or decodedInstruction=SR0 or decodedInstruction=SL1 or decodedInstruction=SR1 else
				Format2 when decodedInstruction=ADDI or decodedInstruction=SUBI or decodedInstruction=LDL or decodedInstruction=LDH else
				other;
				
	decodedInstruction <=   ADD     when ir(15 downto 12) = x"0" else
                            SUB     when ir(15 downto 12) = x"1" else
                            AAND    when ir(15 downto 12) = x"2" else
                            OOR     when ir(15 downto 12) = x"3" else
                            XXOR    when ir(15 downto 12) = x"4" else
                            ADDI    when ir(15 downto 12) = x"5" else
                            SUBI    when ir(15 downto 12) = x"6" else
                            LDL     when ir(15 downto 12) = x"7" else
                            LDH     when ir(15 downto 12) = x"8" else
                            LD      when ir(15 downto 12) = x"9" else
                            ST      when ir(15 downto 12) = x"A" else
                            SL0     when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"0" else
                            SL1     when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"1" else
                            SR0     when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"2" else
                            SR1     when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"3" else
                            NOT_A   when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"4" else
                            NOP     when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"5" else
                            HALT    when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"6" else
                            LDSP    when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"7" else
                            RTS     when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"8" else
                            POP     when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"9" else
                            PUSH    when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"A" else 
                            MUL     when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"B" else        -- MULTIPLICACAO
                            DIV     when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"C" else        -- DIVISAO
                            MFH     when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"D" else        -- MOVE FROM HIGH
                            MFL     when ir(15 downto 12) = x"B" and ir(3 downto 0) = x"E" else        -- MOVE FROM LOW
               
                            -- Jump instructions (18). 
                            -- Here the status flagss are tested to jump or not
                            JUMP_R  when ir(15 downto 12) = x"C" and (
                                     ir(3 downto 0) = x"0" or                           -- JMPR
                                    (ir(3 downto 0) = x"1" and negativeflag = '1') or   -- JMPNR
                                    (ir(3 downto 0) = x"2" and zeroflag = '1') or       -- JMPZR
                                    (ir(3 downto 0) = x"3" and carryflag = '1') or      -- JMPCR
                                    (ir(3 downto 0) = x"4" and overflowflag = '1')      -- JMPVR
                                    ) else 

                            JUMP_A  when ir(15 downto 12) = x"C" and (
                                     ir(3 downto 0) = x"5" or                           -- JMP
                                    (ir(3 downto 0) = x"6" and negativeflag = '1') or   -- JMPN
                                    (ir(3 downto 0) = x"7" and zeroflag = '1') or       -- JMPZ
                                    (ir(3 downto 0) = x"8" and carryflag = '1') or      -- JMPC
                                    (ir(3 downto 0) = x"9" and overflowflag = '1')      -- JMPV
                                    ) else 

                            JUMP_D  when ir(15 downto 12) = x"D" or (                           -- JMPD
                                        ir(15 downto 12) = x"E" and ( 
                                            (ir(11 downto 10) = "00" and negativeflag = '1') or -- JMPND
                                            (ir(11 downto 10) = "01" and zeroflag = '1') or     -- JMPZD
                                            (ir(11 downto 10) = "10" and carryflag = '1') or    -- JMPCD
                                            (ir(11 downto 10) = "11" and overflowflag = '1')    -- JMPVD
                                        )   
                                    )  else 

                            JSRR  when ir(15 downto 12) = x"C" and ir(3 downto 0) = x"A" else
                            JSR   when ir(15 downto 12) = x"C" and ir(3 downto 0) = x"B" else
			    PUSHF when ir(15 downto 12) = x"C" and ir(3 downto 0) = x"C" else
			    POPF  when ir(15 downto 12) = x"C" and ir(3 downto 0) = x"D" else
			    RTI   when ir(15 downto 12) = x"C" and ir(3 downto 0) = x"E" else
                            JSRD  when ir(15 downto 12) = x"F" else

                            NOP ;   -- IMPORTANT: default condition in case of conditional jumps with corresponding flags = '0';
    
    process(clk, rst)
    begin
        if rst = '1' then -- reset 
            currentState <= Sidle;
            PC <= (others=>'0');
            IR <= (others=>'0');
            SP <= (others=>'0'); -- arrumar   
            RA <= (others=>'0');
			RB <= (others=>'0');  
            RULA <= (others=>'0');
			flags <= (others=>'0');
            high <= (others=>'0');
            low <= (others=>'0');
			-- zera o banco de registradores
			for i in 0 to 15 loop   
                registerFile(i) <= (others=>'0');  
            end loop;
        elsif rising_edge(clk) then -- sensivel a borda de subida do clock  
            
            if interruptReg /= '1' then
		        interruptReg <= intr;
	        end if;   
            
        	case currentState is
                when Sidle =>  
                    currentState <= Sfetch; 
                
                when Sfetch => -- busca da instrucao
                    if interruptReg = '1' and atendendo_interrupcao = false then
                        atendendo_interrupcao <= true;
                        currentState <= Sinterrupt;
                    else -- salto para a interrupçao
                    	PC <= PC + 1; -- PC++
                        IR <= data_in; -- IR <= MEM(PC)
                        currentState <= Sreg;
                    end if; 
					
                when Sreg => -- leitura dos registradores
        			RA <= S1;
        			RB <= S2;
        			if decodedInstruction = HALT then
        				currentState <= Shalt;
        			elsif decodedInstruction = PUSHF then
        		 		currentState <= Spushf; 
        		 	elsif decodedInstruction = POPF then
        		 		currentState <= Spopf;
                    else 
                        currentState <= Salu;
                    end if;
                    
                when Salu => -- operacao com a ULA
        			RULA <= outula; -- recebe o resultado da operacao com a ULA
        			-- atualizacao das flags
        			if (decodedInstruction = ADD or decodedInstruction = ADDI or decodedInstruction = SUB or decodedInstruction = SUBI) then 
        				carryflag <= c;
        				overflowflag <= v;
        			end if;
        			if (instType = Format1 or decodedInstruction = ADDI or decodedInstruction = SUBI) then
        				negativeflag <= n;
        				zeroflag <= z;
        			end if;
        			
        			if decodedInstruction = PUSH then
        				currentState <= Spush;
        			elsif decodedInstruction = POP then   
        				currentState <= Spop;
        			elsif decodedInstruction = RTS then   
        				currentState <= Srts;
        		 	elsif decodedInstruction = RTI then
        		 		currentState <= Srti;
        			elsif decodedInstruction = LDSP then   
        				currentState <= Sldsp;
        			elsif decodedInstruction = LD then   
        				currentState <= Sld;
        			elsif decodedInstruction = ST then   
        				currentState <= Sst;
        			elsif instType = Format1 or instType = Format2 then   
        				currentState <= Swbk;	
        			elsif decodedInstruction = JUMP_R or decodedInstruction = JUMP_A or decodedInstruction = JUMP_D then   
        				currentState <= Sjmp;
        			elsif decodedInstruction = JSRR or decodedInstruction = JSR or decodedInstruction = JSRD then   
        				currentState <= Ssbrt;
                    elsif decodedInstruction = MUL then
                        currentState <= Smul;
                    elsif decodedInstruction = DIV then
                        currentState <= Sdiv;
                    elsif decodedInstruction = MFH then
                        currentState <= Smfh;
                    elsif decodedInstruction = MFL then
                        currentState <= Smfl;
        			else    -- ** ATTENTION ** NOP and jumps with corresponding flags=0 execute in just 3 clock cycles 
        				currentState <= Sfetch;   
        			end if;     
                    
        		when Swbk => -- ciclo final para as intruções logicas/aritméticas (write back)
        			registerFile(TO_INTEGER(UNSIGNED(IR(11 downto 8)))) <= dtReg; -- regFile(i) <= RULA
        			currentState <= Sfetch;
                    
        		when Sld => -- ciclo final para a instrução de load (escrita do valor lido na memória)
        			registerFile(TO_INTEGER(UNSIGNED(IR(11 downto 8)))) <= dtReg; -- regFile(i) <= MEM(RULA)
        			currentState <= Sfetch;
                    
        		when Sst => -- ciclo final para a instrução de store (escrita do valor na memória)
                    currentState <= Sfetch;
                
        		when Sldsp => -- ciclo final para a instrução de load para o stack pointer (escrita do valor no SP)
        			currentState <= Sfetch;
        			SP <= RULA; -- SP <= RegFile(i)
                    
        		when Spush => -- ciclo final para a instrução de push (colocar dado na pilha e decrementar SP)
        			SP <= SP - 1;
        			currentState <= Sfetch;
                    
                when Spop => -- ciclo final para a instrução de pop (retirar dado da pilha e incrementar SP)
        			SP <= RULA; -- SP++
        			registerFile(TO_INTEGER(UNSIGNED(IR(11 downto 8)))) <= dtReg; -- regFile(i) <= MEM(SP)
        			currentState <= Sfetch;
                    
        		when Ssbrt => -- ciclo final para as instruções de subrotina (colocar PC na pilha e saltar)
        			PC <= RULA; -- atualiza o PC com o novo endereço
        			SP <= SP - 1;
        			currentState <= Sfetch;
                    
        		when Sjmp => -- ciclo final para as instruções de salto (saltar para o endereço especificado)
        			PC <= RULA; -- atualiza PC
        			currentState <= Sfetch;
                    
        		when Srts => -- ciclo final para a instrução de retorno de subrotina (recuperar pc da pilha e incrementar SP)
        			SP <= RULA; -- SP++
        			PC <= data_in; -- PC recuperado da pilha
        			currentState <= Sfetch; 
                
                when Smul =>
                    high <= multiply(31 downto 16);
                    low <= multiply(15 downto 0);
                    currentState <= Sfetch;
                    
                when Sdiv =>
                    high <= division(31 downto 16);
                    low <= division(15 downto 0);
                    currentState <= Sfetch;
                    
                when Smfh =>
                    registerFile(TO_INTEGER(UNSIGNED(IR(11 downto 8)))) <= dtReg;
                    currentState <= Sfetch;
                
                when Smfl =>
                    registerFile(TO_INTEGER(UNSIGNED(IR(11 downto 8)))) <= dtReg;
                    currentState <= Sfetch;
                            
        		when SpushF =>
        			SP <= SP - 1;
        			currentState <= Sfetch;
        				 
        		when SpopF =>
        			SP <= SP + 1;
        			currentState <= Sfetch;
        			flags <= dtReg(3 downto 0);
        				 
        		when Srti =>
        			SP <= RULA; -- SP++
        			PC <= data_in; -- PC recuperado da pilha
        			interruptReg <= '0';
					atendendo_interrupcao <= false;
        			currentState <= Sfetch;  
        		when Sinterrupt => -- atendendo interrupcao
        			SP <= SP - 1;
        			PC <= INTERRUPT_ADDRESS;
					currentState <= Sfetch;
        		when others => -- Shalt
        			currentState <= Shalt;
		
            end case;
        end if;
    end process;		
	
	-- seleciona o destino do dado que será escrito no banco de registradores
	dtReg <= data_in when decodedInstruction = LD or decodedInstruction = POP or decodedInstruction = POPF else -- dado da memória
		     RULA; -- dado da ULA
	
	
	
	-- Register File read
	-- Selects the read register 1 (Rsource 1)	
	S1 <= registerFile(TO_INTEGER(UNSIGNED(IR(7 downto 4))));
	
    -- Selects the read register 2 (Rtarget or Rsource2)
    S2 <= registerFile(TO_INTEGER(UNSIGNED(ir(11 downto 8)))) when instType = Format2 or decodedInstruction = PUSH or currentState = Sst or decodedInstruction = MUL or decodedInstruction = DIV else -- RTarget
		  registerFile(TO_INTEGER(UNSIGNED(IR(3 downto 0)))); -- RSource2
	
	
	
	-- operandos do somador -- usado para ADD/SUB/LD/ST/ADDI/SUBI/JSRD/JUMP_D
	op1_adder <= opA when decodedInstruction = ADD or decodedInstruction = SUB or decodedInstruction = LD or decodedInstruction = ST else -- ADD/SUB/LD/ST
				 x"00" & IR(7 downto 0) when decodedInstruction = ADDI else -- ADDI
				 (not(x"00" & IR(7 downto 0))+1) when decodedInstruction = SUBI else -- SUBI
				 opA(11) & opA(11) & opA(11) & opA(11) & opA(11 downto 0) when decodedInstruction = JSRD else -- JSRD
				 opA(9) & opA(9) & opA(9) & opA(9) & opA(9) & opA(9) & opA(9 downto 0); -- JUMP_D
				 
	op2_adder <= (not(opB)+1) when decodedInstruction = SUB else -- SUB
				 opB; -- ADD/LD/ST/ADDI/SUBI/JSRD/JUMP_D
				 
	adder <= ('0' & op1_adder) + ('0' & op2_adder); -- ADD/SUB/SUBI/LD/ST/JSRD/JUMP_D
	
	-- Operandos da ULA
	opA <= IR when instType = Format2 or decodedInstruction = JUMP_D or decodedInstruction = JSRD else -- constante imediata que está nos bits menos significativos do IR
		   RA; -- registrador lido
		   
	-- escolhe o segundo operando da ULA
	opB <= SP when decodedInstruction = RTS or decodedInstruction = POP else -- SP
		   PC when decodedInstruction = JUMP_R or decodedInstruction = JUMP_A or decodedInstruction = JUMP_D or decodedInstruction = JSRR or decodedInstruction = JSR or decodedInstruction = JSRD else -- PC
		   RB; -- registrador lido
    
    -- ALU
    outula <=   opB(15 downto 8) & opA(7 downto 0) when decodedInstruction = LDL else 			-- LDL
                opA(7 downto 0) & opB(7 downto 0) when decodedInstruction = LDH else 			-- LDH
				opA xor opB when decodedInstruction = XXOR else 								-- XOR
				opA and opB when decodedInstruction = AAND else 								-- AND
				opA or opB when decodedInstruction = OOR else 									-- OR
				opA(14 downto 0) & '0' when decodedInstruction = SL0   else  					-- shift left 1 posição para a esquerda inserindo 0 no bit 0
				opA(14 downto 0) & '1' when decodedInstruction = SL1   else  					-- shift left 1 posição para a esquerda inserindo 1 no bit 0
				'0' & opA(15 downto 1) when decodedInstruction = SR0   else	 					-- shift right 1 posição para a direita inserindo 0 no bit 15
				'1' & opA(15 downto 1) when decodedInstruction = SR1   else						-- shift right 1 posição para a direita inserindo 0 no bit 15
				not opA                when decodedInstruction = NOT_A  else					-- NOT 
				STD_LOGIC_VECTOR(UNSIGNED(opB) + 1)	when decodedInstruction = RTS or decodedInstruction = POP or decodedInstruction = RTI else   									-- Incrementa o SP para POP e RTS
				opA                               	when decodedInstruction = JUMP_A or decodedInstruction = JSR  or decodedInstruction = LDSP else		-- bypass para jump absoluto, salto de subrotina ou carregamento de SP
                		high                                	when decodedInstruction = MFH else
               			low                                 	when decodedInstruction = MFL else     
				adder(15 downto 0); 																-- ADD, LD, ST, SUB, ADDI, SUBI, JSRD, JUMP_D
		
    -- Flags para a ULA
    	z <= '1' when outula = 0 else '0'; 										-- zero
	n <= outula(15); 														-- negativo			
	c <= adder(16); 														-- carry
	v <= '1' when (op1_adder(15) = op2_adder(15) and op1_adder(15) /= outula(15)) else '0'; 	-- overflow sinalization
       
        
    -- MULTIPLY    
    multiply <= RA * RB;
    
    -- DIVISION
    division(31 downto 16) <= STD_LOGIC_VECTOR(UNSIGNED(RB) mod UNSIGNED(RA));
    division(15 downto 0) <= STD_LOGIC_VECTOR(UNSIGNED(RB)/UNSIGNED(RA));        
    
    
	-- Memory
    
	-- Memory Address
	address <= PC when currentState = Sfetch else -- Busca da instruçao
			   RULA when currentState = Sld or currentState = Sst or currentState = Spop or currentState = Srts else -- LD/ST/RTS/POP
		       SP + 1 when currentState = SpopF else	
		       SP; -- PUSH or PUSHF or Sinterrupt
			   
	-- Data out
	data_out <= S2 when currentState = Sst else -- ST
		        x"000" & flags when currentState = SpushF else -- PUSHF
				PC when currentState = Sinterrupt else
		        opB; -- PUSH/Salto subrotina/Sinterrupt
	
    -- Memory signals
    ce <= '1' when rst = '0' and (currentState = Sfetch or currentState = Srts or currentState = Srti or 
           currentState = Spop or currentState = Spopf or currentState = Sld or currentState = Ssbrt or 
           currentState = Spush or currentState = Spushf or currentState = Sst or
		   currentState = Sinterrupt) else '0';
		   
    rw <= '1' when (currentState = Sfetch or currentState = Srts or currentState = Spop or 
           currentState = Spopf or currentState = Sld) else '0';
end behavioral;
