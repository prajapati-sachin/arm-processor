
---------------------------------------------------------------------------------------------------------------
--ALU
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity alu is
    port(
        op1 : IN  std_logic_vector(31 downto 0);
        op2 : IN  std_logic_vector(31 downto 0);
	operation : IN std_logic_vector(3 downto 0);        
	carry : IN std_logic;	
	result : OUT std_logic_vector(31 downto 0);
	flags : OUT std_logic_vector(3 downto 0)        
	);
end alu;

architecture alu of alu is
signal c : std_logic_vector (1 downto 0):="00";
signal flag : std_logic_vector (3 downto 0):="0000";
signal resultl : std_logic_vector(31 downto 0);  
begin
    with operation select
        resultl <= op1 and op2 when "0000", --and
                   op1 xor op2 when "0001", --eor
                   op1 + (not op2) + 1 when "0010", --sub
                   (not op1) + op2 + 1 when "0011", --rsb
                   op1 + op2 when "0100", --add
                   op1 + op2 + carry when "0101", --adc
                   op1 + (not op2) + carry when "0110", --sbc
                   (not op1) + op2 + carry when "0111", --rsc
                   op1 and op2 when "1000", --tst
                   op1 xor op2 when "1001", --teq
                   op1 + (not op2) + 1 when "1010", --cmp
                   op1 + op2 when "1011", --cmn
                   op1 or op2 when "1100", --orr
                   op2 when "1101", --mov
                   op1 and (not op2) when "1110", --bic
                   (not op2) when others; --mvn
                   
--    with operation select
--       result <= resultl when "0000", --and
--                 resultl when "0001", --eor
--                 resultl when "0010", --sub
--                 resultl when "0011", --rsb
--                 resultl when "0100", --add
--                 resultl when "0101", --adc
--                 resultl when "0110", --sbc
--                 resultl when "0111", --rsc
--                 resultl when "1100", --orr
--                 resultl when "1101", --mov
--                 resultl when "1110", --bic
--                 resultl when "1111"; --mvn
                result <= resultl;
                            

             --Flag order = N Z C V
             --Flag(0)-V
             --Flag(1)-C
             --Flag(2)-Z
             --Flag(3)-N
                   
             c(0) <= (op1(31) xor op2(31)) xor resultl(31);
             c(1) <= (op1(31) and op2(31)) or (op1(31) and c(0)) or (op2(31) and c(0));
             flag(1) <= c(1);
             flag(0) <= c(0) xor c(1);
             with resultl select
                flag(2) <= '0' when "00000000000000000000000000000000" ,
                    '1' when others;
             
             flag(3) <= resultl(31);  
--             if result = "00000000000000000000000000000000" then
--                 flag(2) <= '1';
--             end if    
--             if result(31) = '1' then
--                 flag(3) <= '1';    
--             flags(3 downto 0) <= flag(3 downto 0); 
	         flags<= flag;
      
            
end alu;

---------------------------------------------------------------------------------------------------------------
--MULTIPLIER
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity multiplier is
    port(
        op1 : IN  std_logic_vector(31 downto 0);
        op2 : IN  std_logic_vector(31 downto 0);	
	result : OUT std_logic_vector(31 downto 0);
	flags : OUT std_logic_vector(3 downto 0)        
	);
end multiplier;

architecture multiplier of multiplier is
signal c : std_logic_vector (1 downto 0):="00";
signal flag : std_logic_vector (3 downto 0):="0000";
signal resultl : std_logic_vector(31 downto 0);  
begin
   resultl <= op1 * op2;
   result <= resultl;
                            

                   
         --Flag order = N Z C V
         --Flag(0)-V
         --Flag(1)-C
         --Flag(2)-Z
         --Flag(3)-N
                  
            c(0) <= (op1(31) xor op2(31)) xor resultl(31);
            c(1) <= (op1(31) and op2(31)) or (op1(31) and c(0)) or (op2(31) and c(0));
            flag(1) <= c(1);
            flag(0) <= c(0) xor c(1);
            with resultl select
               flag(2) <= '0' when "00000000000000000000000000000000" ,
                   '1' when others;
            
            flag(3) <= resultl(31);  
--             if result = "00000000000000000000000000000000" then
--                 flag(2) <= '1';
--             end if    
--             if result(31) = '1' then
--                 flag(3) <= '1';    
--             flags(3 downto 0) <= flag(3 downto 0); 
            flags<= flag;

	
end multiplier;

-----------------------------------------------------------------------------------------------------------
--SHIFTER
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
--USE ieee.std_logic_unsigned.all;

entity shifter is
    port(
        op1 : IN  std_logic_vector(31 downto 0);
        shift_type : IN std_logic_vector(1 downto 0); 
        shift_amount : IN std_logic_vector(4 downto 0);       
        shift_carry : OUT std_logic;	
        result : OUT std_logic_vector(31 downto 0)     
	);
end shifter;

architecture shifter of shifter is
signal c : std_logic;
signal a : std_logic_vector (31 downto 0);
signal b : std_logic_vector (31 downto 0);
signal amount : integer range 0 to 31:= 0;
signal flag : std_logic_vector (3 downto 0):="0000"; 
--Shift type
--00 = logical left
--01 = logical right
--10 = arithmetic right
--11 = rotate right
--


begin
    a <= op1;
    amount <= to_integer(unsigned(shift_amount));
    with shift_type select
    b <= std_logic_vector(shift_left(unsigned(op1), amount)) when "00",
         std_logic_vector(shift_right(unsigned(op1), amount)) when "01",
         std_logic_vector(shift_right(signed(op1), amount)) when "10",
         std_logic_vector(rotate_right(unsigned(op1), amount)) when others;

--    with shift_type select
--    c <=  a(32-amount) when "00", --logical left
--          a(amount-1) when "01", --logical right
--          a(amount-1) when "10", --arithmetic right
--          a(amount-1) when others; --rotate right
          

--    with shift_type select
    c <=  '0' when amount=0 else 
          a(32-amount) when shift_type = "00" else  --logical left
          a(amount-1) when shift_type = "01" else --logical right
          a(amount-1) when shift_type = "10" else--arithmetic right
          a(amount-1) when shift_type ="11"; --rotate right




    result <= b;
    shift_carry <= c;

--Earlier version using generate    
--    GEN_REG: 
--   for I in 0 to (31-amount) generate
--      b(I) <= a(I+amount);
--   end generate GEN_REG; 
--   GEN_REG1: 
--      for I in 0 to (amount-1) generate
--         with shift_type select
--            b(31 - amount + I + 1) <= '0' when "00",
--                     a(31) when "01",
--                     a(I) when "11";    
--      end generate GEN_REG1; 
--    shift_carry <= a(amount-1);
--    result <= b;

end shifter;

---------------------------------------------------------------------------------------------------------------
--REGISTER
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity register_file is
port(
    wr_data : IN  std_logic_vector(31 downto 0);
    wr_addr : IN std_logic_vector(3 downto 0); 
    rd_addr1 : IN std_logic_vector(3 downto 0); 
    rd_addr2 : IN std_logic_vector(3 downto 0); 
    write_enable : IN std_logic;
    clock : IN std_logic;
    reset : IN std_logic;	
	rd_data1 : OUT std_logic_vector(31 downto 0) ; 
	rd_data2 : OUT std_logic_vector(31 downto 0)     
	);
end register_file;

architecture register_file of register_file is

type registerFile is array(0 to 15) of std_logic_vector(31 downto 0);
signal registers : registerFile := (others => (others => '0'));
signal num1 : Integer; 
signal num2 : Integer; 
begin
    rd_data1 <= registers(to_integer(unsigned(rd_addr1)));
    rd_data2 <= registers(to_integer(unsigned(rd_addr2)));
    process(clock, reset)
       begin
       if clock'event and clock = '1' then
            if write_enable = '1' then 
                registers(to_integer(unsigned(wr_addr)))<= wr_data;
            end if;
        end if;
        if reset = '1' then 
            registers(0) <= "00000000000000000000000000000000";
            registers(1) <= "00000000000000000000000000000000";
            registers(2) <= "00000000000000000000000000000000";
            registers(3) <= "00000000000000000000000000000000";
            registers(4) <= "00000000000000000000000000000000";
            registers(5) <= "00000000000000000000000000000000";
            registers(6) <= "00000000000000000000000000000000";
            registers(7) <= "00000000000000000000000000000000";
            registers(8) <= "00000000000000000000000000000000";
            registers(9) <= "00000000000000000000000000000000";
            registers(10) <= "00000000000000000000000000000000";
            registers(11) <= "00000000000000000000000000000000";
            registers(12) <= "00000000000000000000000000000000";
            registers(13) <= "00000000000000000000000000000000";
            registers(14) <= "00000000000000000000000000000000";
            registers(15) <= "00000000000000000000000000000000";
        end if;   
    end process;
   
end register_file;



---------------------------------------------------------------------------------------------------------------
--PROCESSOR MEMORY PATH
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity p_m_path is
    port(
    processor_input : IN  std_logic_vector(31 downto 0);
    memory_input : IN std_logic_vector(31 downto 0); 
    inst_type: IN std_logic_vector(3 downto 0); 
    byte_offset : IN std_logic_vector(1 downto 0);  
    processor_output : OUT std_logic_vector(31 downto 0) ;
    memory_output : OUT std_logic_vector(31 downto 0) ; 
    write_enables : OUT std_logic_vector(1 downto 0)     
    );
end p_m_path;

architecture p_m_path of p_m_path is
signal a : std_logic_vector(31 downto 0);
signal b : std_logic_vector(31 downto 0);
signal c : std_logic_vector(31 downto 0);
signal d : std_logic_vector(31 downto 0);

begin

-- 4 bit inst_type inst_type(3) for sign
--                 inst_type(0) for sign
--                 inst_type(2 downto 1) = 00 for word
--                                         01 for half word
--                                         10 for byte
-- 😎 😎 😎 😎 😎 😎 😎
    with byte_offset select 
        a(7 downto 0) <= memory_input(7 downto 0) when "00",
                         memory_input(15 downto 8) when "01",
                         memory_input(23 downto 16) when "10",
                         memory_input(31 downto 24) when others; 
      generate1: 
        for I in 8 to 31 generate
             with inst_type(3) select
                a(I) <= '0' when '0',
                         a(7) when others;   
        end generate generate1;  
                        
    with byte_offset select 
        b(15 downto 0)<= memory_input(15 downto 0) when "00",
                         memory_input(31 downto 16) when others;
       generate2: 
         for I in 16 to 31 generate
              with inst_type(3) select
                 b(I) <= '0' when '0',
                          b(15) when others;   
         end generate generate2; 
         
    with byte_offset select 
         c(7 downto 0) <= processor_input(7 downto 0) when "00",
                          processor_input(15 downto 8) when "01",
                          processor_input(23 downto 16) when "10",
                          processor_input(31 downto 24) when others;
    c(15 downto 8) <=  c(7 downto 0);
    c(23 downto 16) <=  c(7 downto 0);
    c(31 downto 24) <=  c(7 downto 0); 
    
    with byte_offset select 
         d(15 downto 0)<= processor_input(15 downto 0) when "00",
                          processor_input(31 downto 16) when others;
    d(31 downto 16) <=  d(15 downto 0);
  
    
    with inst_type(2 downto 0) select
         processor_output <= memory_input when "000",
                             b            when "010",
                             a            when others;
                    
    with inst_type(2 downto 0) select
         memory_output <= processor_input when "001",
                             d            when "011",
                             c            when others;
    
    with inst_type(2 downto 1) select
        write_enables <= "00" when "00",--word
                         "01" when "01",-- half word
                         "10" when others;--byte
    
   
   
end p_m_path;

---------------------------------------------------------------------------------------------------------------
-- MEMORY
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity memory is
port(
    write_enables : std_logic_vector(1 downto 0);
    wr_data : IN  std_logic_vector(31 downto 0);
    addr : IN std_logic_vector(5 downto 0); 
    write_enable : IN std_logic;
    read_enable : IN std_logic;
    clock : IN std_logic;	
	rd_data : OUT std_logic_vector(31 downto 0) 
	    
	);
end memory;
--00 for word 01 for hLF WORD 10 FOR BYTE
architecture data_memory of memory is
type memory_array is array(0 to 63) of std_logic_vector(31 downto 0);
signal data_memory : memory_array := (others => (others => '0'));
signal address : Integer; 
begin
    address <= to_integer(unsigned(addr));
    process(wr_data,clock,address,write_enable,read_enable,write_enables)
        begin
        if read_enable = '1' then
            rd_data <= data_memory(address);
        end if;  
        if write_enable ='1' then
            if clock'event and clock = '1' then
                if write_enables = "00" then
                    data_memory(address) <= wr_data;
                elsif write_enables = "01" then
                    data_memory(address)(15 downto 0) <= wr_data(15 downto 0);
                elsif write_enables = "10" then
                    data_memory(address)(7 downto 0) <= wr_data(7 downto 0);
                end if;
            end if;
        end if;
    end process;

end data_memory;


---------------------------------------------------------------------------------------------------------------
--PROGRAM COUNTER
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity program_counter is
port(
    wr_data : IN  std_logic_vector(5 downto 0);
    
    write_enable : IN std_logic;
    read_enable : IN std_logic;
    clock : IN std_logic;	
	rd_data : OUT std_logic_vector(5 downto 0) 
	    
	);
end program_counter;

architecture program_counter of program_counter is
signal pc : std_logic_vector(5 downto 0);
begin
    process(wr_data,clock,write_enable,read_enable)
        begin
        if read_enable = '1' then
            rd_data <= pc;
        end if;  
        if write_enable ='1' then
            if clock'event and clock = '1' then
                pc <= wr_data;
            end if;
        end if;
    end process;

end program_counter;


---------------------------------------------------------------------------------------------------------------
--MUX_2_3bit
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity mux_2_5 is
port(
    data1: IN  std_logic_vector(5 downto 0);
    data2: IN  std_logic_vector(5 downto 0);	
    cntrl : IN std_logic;
	data : OUT std_logic_vector(5 downto 0)  
	);
end mux_2_5;

architecture mux of mux_2_5 is
begin
    with cntrl select
        data <= data1 when '0',
                data2 when others;
end mux;

---------------------------------------------------------------------------------------------------------------
--MUX_2_3bit
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity mux_2_3 is
port(
    data1: IN  std_logic_vector(3 downto 0);
    data2: IN  std_logic_vector(3 downto 0);	
    cntrl : IN std_logic;
	data : OUT std_logic_vector(3 downto 0)  
	);
end mux_2_3;

architecture mux of mux_2_3 is
begin
    with cntrl select
        data <= data1 when '0',
                data2 when others;
end mux;

---------------------------------------------------------------------------------------------------------------
--MUX_2_31bit
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity mux_2_31 is
port(
    data1: IN  std_logic_vector(31 downto 0);
    data2: IN  std_logic_vector(31 downto 0);	
    cntrl : IN std_logic;
	data : OUT std_logic_vector(31 downto 0)  
	);
end mux_2_31;

architecture mux of mux_2_31 is
begin
    with cntrl select
        data <= data1 when '0',
                data2 when others;
end mux;

---------------------------------------------------------------------------------------------------------------
-- MUX_4
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity mux_4 is
port(
    data1: IN  std_logic_vector(31 downto 0);
    data3: IN  std_logic_vector(31 downto 0);
    data4: IN  std_logic_vector(31 downto 0);	
    cntrl : IN std_logic_vector(1 downto 0);
	data : OUT std_logic_vector(31 downto 0)  
	);
end mux_4;

architecture mux of mux_4 is
signal data2: std_logic_vector(31 downto 0):="00000000000000000000000000000100";
begin
    with cntrl select
        data <= data1 when "00",
                data2 when "01",
                data3 when "10",
                data4 when others;
end mux;
---------------------------------------------------------------------------------------------------------------
-- REGISTER
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity register_31 is
port(
    wr : IN  std_logic_vector(31 downto 0);
    enable: IN std_logic;
    clock : IN std_logic;	
	rd : OUT std_logic_vector(31 downto 0)  
	);
end register_31;

architecture register_31 of register_31 is
signal data : std_logic_vector(31 downto 0);
begin
    rd <= data;
    process(wr,clock,enable)
        begin
        if clock'event and clock = '1' then
            if enable ='1' then
                data <= wr;
            end if;
        end if;
    end process;

end register_31;

---------------------------------------------------------------------------------------------------------------
--S2
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity s2 is
port(
    data1: IN  std_logic_vector(23 downto 0);	
	data : OUT std_logic_vector(31 downto 0)  
	);
end s2;

architecture s2 of s2 is
begin
    generate1:
        for i in 0 to 23 generate
            data(i+2) <= data1(i);
        end generate generate1;
    generate2:
        for i in 24 to 29 generate
                data(i+2) <= data1(23);
        end generate generate2;
    data(0) <= '0';
    data(1) <= '0';
    
end s2;

---------------------------------------------------------------------------------------------------------------
--EX
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity ex is
port(
    data1: IN  std_logic_vector(11 downto 0);	
	data : OUT std_logic_vector(31 downto 0) := (others => '0') 
	);
end ex;

architecture ex of ex is
begin
    data(11 downto 0) <= data1;
    
end ex;



---------------------------------------------------------------------------------------------------------------
--MAIN ENTITY
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity datapath is
PORT ( 
	clock, reset : in std_logic;
	ins          : out std_logic_vector(31 downto 0);
	F            : out std_logic_vector(3 downto 0);
	PW           : in  std_logic;
	IorD         : in  std_logic;
	MR           : in  std_logic;
	MW           : in  std_logic;
	IW           : in  std_logic;
	DW           : in  std_logic;
	Rsrc         : in  std_logic;
	Shi          : in  std_logic; --shiftamount from register
	Shift        : in  std_logic; --shift( from register '1' and from intruction '0')
	Wsrc         : in  std_logic;
	M2R          : in  std_logic;
	RW           : in  std_logic;
	AW           : in  std_logic;
	BW           : in  std_logic;
	Asrc1        : in  std_logic;
	Asrc2        : in  std_logic_vector(1 downto 0);
	MorA        : in  std_logic;
	Fset         : in  std_logic;
	alu_op       : in  std_logic_vector(3 downto 0);
	p_m_path_op  : in   std_logic_vector(3 downto 0);
	byte_offset  : in std_logic_vector(1 downto 0);
	ReW          : in  std_logic;
    shift_type : IN std_logic_vector(1 downto 0)  
        
);
end datapath;

architecture datapath of datapath is

component alu is
port(
        op1 : IN  std_logic_vector(31 downto 0);
        op2 : IN  std_logic_vector(31 downto 0);
        operation : IN std_logic_vector(3 downto 0);        
        carry : IN std_logic;	
        result : OUT std_logic_vector(31 downto 0);
        flags : OUT std_logic_vector(3 downto 0)        
	);
end component;

component shifter is
port(
        op1 : IN  std_logic_vector(31 downto 0);
        shift_type : IN std_logic_vector(1 downto 0); 
        shift_amount : IN std_logic_vector(4 downto 0);       
        shift_carry : OUT std_logic;	
        result : OUT std_logic_vector(31 downto 0)     
	);
end component;

component multiplier is
port(
        op1 : IN  std_logic_vector(31 downto 0);
        op2 : IN  std_logic_vector(31 downto 0);	
        result : OUT std_logic_vector(31 downto 0);
        flags : OUT std_logic_vector(3 downto 0)        
	);
end component;

component p_m_path is
port(
        processor_input : IN  std_logic_vector(31 downto 0);
        memory_input : IN std_logic_vector(31 downto 0); 
        inst_type: IN std_logic_vector(3 downto 0); 
        byte_offset : IN std_logic_vector(1 downto 0); 	
        processor_output : OUT std_logic_vector(31 downto 0) ;
        memory_output : OUT std_logic_vector(31 downto 0) ; 
        write_enables : OUT std_logic_vector(1 downto 0)     
	);
end component;

component register_file is
port(
        wr_data : IN  std_logic_vector(31 downto 0);
        wr_addr : IN std_logic_vector(3 downto 0); 
        rd_addr1 : IN std_logic_vector(3 downto 0); 
        rd_addr2 : IN std_logic_vector(3 downto 0); 
        write_enable : IN std_logic;
        clock : IN std_logic;
        reset : IN std_logic;	
        rd_data1 : OUT std_logic_vector(31 downto 0) ; 
        rd_data2 : OUT std_logic_vector(31 downto 0)     
	);
end component;

component memory is
port(
        write_enables : std_logic_vector(1 downto 0);
        wr_data : IN  std_logic_vector(31 downto 0);
        addr : IN std_logic_vector(5 downto 0); 
        write_enable : IN std_logic;
        read_enable : IN std_logic;
        clock : IN std_logic;    
        rd_data : OUT std_logic_vector(31 downto 0)  
	);
end component;

component program_counter is
port(
        wr_data : IN  std_logic_vector(5 downto 0);
        write_enable : IN std_logic;
        read_enable : IN std_logic;
        clock : IN std_logic;	
        rd_data : OUT std_logic_vector(5 downto 0) 
	);
end component; 

component mux_2_5 is
port(
        data1: IN  std_logic_vector(5 downto 0);
        data2: IN  std_logic_vector(5 downto 0);	
        cntrl : IN std_logic;
        data : OUT std_logic_vector(5 downto 0)  
	);
end component;

component mux_2_3 is
port(
        data1: IN  std_logic_vector(3 downto 0);
        data2: IN  std_logic_vector(3 downto 0);	
        cntrl : IN std_logic;
        data : OUT std_logic_vector(3 downto 0)  
	);
end component;

component mux_2_31 is 
port(
        data1: IN  std_logic_vector(31 downto 0);
        data2: IN  std_logic_vector(31 downto 0);	
        cntrl : IN std_logic;
        data : OUT std_logic_vector(31 downto 0)  
	);
end component;

component mux_4 is
port(
        data1: IN  std_logic_vector(31 downto 0);
        data3: IN  std_logic_vector(31 downto 0);
        data4: IN  std_logic_vector(31 downto 0);	
        cntrl : IN std_logic_vector(1 downto 0);
        data : OUT std_logic_vector(31 downto 0)  
	);
end component;

component register_31 is
port(
        wr : IN  std_logic_vector(31 downto 0);
        enable: IN std_logic;
        clock : IN std_logic;	
        rd : OUT std_logic_vector(31 downto 0)     
	);
end component;

component s2 is
    port(
        data1: IN  std_logic_vector(23 downto 0);	
        data : OUT std_logic_vector(31 downto 0)  
	);
end component;

component ex is 
port(
        data1: IN  std_logic_vector(11 downto 0);	
        data : OUT std_logic_vector(31 downto 0) := (others => '0') 
	);
end component;

signal pc_output : std_logic_vector(5 downto 0) := (others => '0') ;
signal memory_input_addr : std_logic_vector(5 downto 0) := (others => '0') ;
signal res_output : std_logic_vector(31 downto 0) := (others => '0') ;
signal memory_input_data : std_logic_vector(31 downto 0) := (others => '0') ;
signal memory_output_data : std_logic_vector(31 downto 0) := (others => '0') ;
signal instruction : std_logic_vector(31 downto 0) := (others => '0') ;
signal dr_output : std_logic_vector(31 downto 0) := (others => '0') ;
signal dr_input : std_logic_vector(31 downto 0) := (others => '0') ;
signal rd_addr1 : std_logic_vector(3 downto 0) := (others => '0') ;
signal rd_addr2 : std_logic_vector(3 downto 0) := (others => '0') ;
signal rd_addr2_1 : std_logic_vector(3 downto 0) := (others => '0') ;
signal wr_addr : std_logic_vector(3 downto 0) := (others => '0') ;
signal immediate_operand : std_logic_vector(11 downto 0) := (others => '0') ;
signal branch_offset : std_logic_vector(23 downto 0) := (others => '0') ;
signal branch_offset_ext : std_logic_vector(31 downto 0) := (others => '0') ;
signal operand_ext: std_logic_vector(31 downto 0) := (others => '0') ;
signal wr_data : std_logic_vector(31 downto 0) := (others => '0') ;
signal rd_data1 : std_logic_vector(31 downto 0) := (others => '0') ;
signal rd_data2 : std_logic_vector(31 downto 0) := (others => '0') ;
signal rd_data1_register : std_logic_vector(31 downto 0) := (others => '0') ;
signal rd_data2_register : std_logic_vector(31 downto 0) := (others => '0') ;
signal alu_input1 : std_logic_vector(31 downto 0) := (others => '0') ;
signal alu_input2 : std_logic_vector(31 downto 0) := (others => '0') ;
signal alu_output : std_logic_vector(31 downto 0) := (others => '0') ;
signal data_addr : std_logic_vector(31 downto 0) := (others => '0') ;
signal flags : std_logic_vector(3 downto 0) := (others => '0') ;
signal write_enable_for_memory : std_logic_vector(1 downto 0) := (others => '0') ;
signal pc_extended : std_logic_vector(31 downto 0) := (others => '0') ;
signal output_asrc1 : std_logic_vector(31 downto 0) := (others => '0') ;
signal output_asrc2 : std_logic_vector(31 downto 0) := (others => '0') ;
signal multiplier_output : std_logic_vector(31 downto 0) := (others => '0') ;
signal final_output : std_logic_vector(31 downto 0) := (others => '0') ;
signal shift_amount : std_logic_vector(4 downto 0) := (others => '0') ;

begin
branch_offset <= instruction(23 downto 0);
immediate_operand <= instruction(11 downto 0);
pc_extended(5 downto 0) <= pc_output(5 downto 0);




alu1 : alu port map
(	op1 => alu_input1,
	op2 => alu_input2, 
	result => alu_output,
	flags => flags,
	operation => alu_op,
	carry => flags(1)	
);

pc : program_counter port map
(   wr_data => final_output(5 downto 0),
    write_enable => PW,
    read_enable => '1',
    clock => clock,
    rd_data => pc_output
);

IorD_mux : mux_2_5 port map
(   data1 => pc_output,
    data2 => res_output(5 downto 0),
    cntrl => IorD,
    data => memory_input_addr
);

memory_unit : memory port map
(       write_enables => write_enable_for_memory ,
        wr_data => memory_input_data,
        addr => memory_input_addr, 
        write_enable => MW,
        read_enable => MR,
        clock => clock,	
        rd_data => memory_output_data

);

register_file_unit : register_file port map
(       wr_data => wr_data,
        wr_addr => wr_addr,
        rd_addr1 => rd_addr1,
        rd_addr2 => rd_addr2,
        write_enable => RW,
        clock => clock,
        reset => reset,	
        rd_data1 => rd_data1, 
        rd_data2 => rd_data2 
    
);


IR : register_31 port map
(   wr => memory_output_data,
    enable => IW,
    clock => clock,
    rd => instruction
);

Mul_read_address1_in_file : mux_2_3 port map
(   data1 => instruction(19 downto 16),
    data2 => instruction(11 downto 8),
    cntrl => Shi,
    data => rd_addr1
);

Mul_read_address2_in_file : mux_2_3 port map
(   data1 => instruction(3 downto 0),
    data2 => instruction(15 downto 12),
    cntrl => Rsrc,
    data => rd_addr2
);    
  
Mul_write_address_in_file : mux_2_3 port map
(   data1 => instruction(15 downto 12),
    data2 => instruction(19 downto 16),
    cntrl => Wsrc,
    data => wr_addr
);  

extension_for_immediate : ex port map 
    (
        data1 => immediate_operand,	
        data => operand_ext
	);  


signextension_for_branch_offset : s2 port map 
    (
        data1 => branch_offset,	
        data => branch_offset_ext
	);

Processor_memory_path : p_m_path port map
(   
    processor_input => rd_data2_register,
    memory_input => memory_output_data, 
    inst_type => p_m_path_op, 
    byte_offset =>  byte_offset, 	
    processor_output => dr_input,
    memory_output => memory_input_data,
    write_enables => write_enable_for_memory    
);

DR : register_31 port map
(   wr => dr_input,
    enable => DW,
    clock => clock,
    rd => dr_output
);

A : register_31 port map
(   wr => rd_data1,
    enable => AW,
    clock => clock,
    rd => rd_data1_register
);

B : register_31 port map
(   wr => rd_data2,
    enable => BW,
    clock => clock,
    rd => rd_data2_register
);
   
Mul_for_asrc1 : mux_2_31 port map
(   data1 => rd_data1_register,
    data2 => pc_extended,
    cntrl => Asrc1,
    data => alu_input1
); 

Mul_for_asrc2 : mux_4 port map
(  data1 => rd_data2_register,
   data3 => operand_ext,
   data4 => branch_offset_ext,    
   cntrl => Asrc2,
   data =>  output_asrc2
); 

with shift select 
    shift_amount <= rd_data1 when '1',
                    instruction(11 downto 7) when others;
       

shifter_unit : shifter port map
(
    op1 => output_asrc2,
    shift_type => shift_type, 
    shift_amount => shift_amount,       
    shift_carry => flags(1),	
    result => alu_input2     
	);
	
multiplier_before_alu : multiplier port map
       (    op1 => alu_input1,
            op2 => alu_input2,    
            result => multiplier_output,
            flags => flags        
        );

Mux_for_final_output : mux_2_31 port map
(   data1 => multiplier_output,
    data2 => alu_output,
    cntrl => MorA,
    data => final_output
);


RES : register_31 port map
(   wr =>final_output,
    enable => ReW,
    clock => clock,
    rd => res_output
);

with Fset select
    F <= flags when '1',
         "0000" when others;

end datapath;








