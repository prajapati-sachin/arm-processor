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
signal op2_new : std_logic_vector(31 downto 0);  


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
                   
    with operation select
       op2_new<=   op2 when "0000", --and
                   op2 when "0001", --eor
                   (not op2)  when "0010", --sub
                   op2  when "0011", --rsb
                   op2 when "0100", --add
                   op2  when "0101", --adc
                   (not op2)  when "0110", --sbc
                   op2  when "0111", --rsc
                   op2 when "1000", --tst
                   op2 when "1001", --teq
                   (not op2)  when "1010", --cmp
                   op2 when "1011", --cmn
                   op2 when "1100", --orr
                   op2 when "1101", --mov
                   op2 when "1110", --bic
                   (not op2) when others; --mvn
                   
                    result <= resultl;
                            
            
             --Flag order = N Z C V
             --Flag(0)-V
             --Flag(1)-C
             --Flag(2)-Z
             --Flag(3)-N
                   
             c(0) <= (op1(31) xor op2_new(31)) xor resultl(31);
             c(1) <= (op1(31) and op2_new(31)) or (op1(31) and c(0)) or (op2_new(31) and c(0));
             flag(1) <= c(1);
             flag(0) <= c(0) xor c(1);
             with resultl select
                flag(2) <= '1' when "00000000000000000000000000000000" ,
                    '0' when others;
             
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
signal temp_result : std_logic_vector (63 downto 0);
signal flag : std_logic_vector (3 downto 0):="0000";
signal resultl : std_logic_vector(31 downto 0);  
begin
   temp_result <= op1 * op2;
   result <= temp_result(31 downto 0);
                            

                   
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
               flag(2) <= '1' when "00000000000000000000000000000000" ,
                   '0' when others;
            
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
        shift_enable : IN std_logic;
        shift_carry : OUT std_logic;  
        result : OUT std_logic_vector(31 downto 0)     
  );
end shifter;

architecture shifter of shifter is
signal c : std_logic;
signal a : std_logic_vector (31 downto 0);
signal b : std_logic_vector (31 downto 0);
signal amount : integer range 0 to 31:= 0;
signal shift_amount1 : std_logic_vector (4 downto 0):="00000";
signal flag : std_logic_vector (3 downto 0):="0000"; 
--Shift type
--00 = logical left
--01 = logical right
--10 = arithmetic right
--11 = rotate right
--


begin
    a <= op1;
    with shift_enable select
        shift_amount1 <= "00000" when '0',
                           shift_amount when others;  
    amount <= to_integer(unsigned(shift_amount1));
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
   -- clock : IN std_logic;
    reset : IN std_logic;
    push_button : IN std_logic;
  rr : OUT std_logic_vector(31 downto 0) ; 
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
    
    rr <= registers(3);
    rd_data1 <= registers(to_integer(unsigned(rd_addr1)));
    rd_data2 <= registers(to_integer(unsigned(rd_addr2)));
    process( write_enable,wr_addr,reset,push_button)
       begin
      -- if clock'event and clock = '1' then
        if push_button = '1' then
            registers(0) <= "00000000000000000000000000000000";
            registers(1) <= "00000000000000000000000000000000";
            registers(2) <= "00000000000000000000000000000010";
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
            if write_enable = '1' then 
                registers(to_integer(unsigned(wr_addr)))<= wr_data;
            end if;
       -- end if;
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
    write_enables : OUT std_logic_vector(3 downto 0)     
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
            processor_output <= a            when "100",
                                b            when "010",
                                memory_input when others;
                       
       with inst_type(2 downto 0) select
            memory_output <=    c            when "101",
                                d            when "011",
                                processor_input      when others;
--    with inst_type(2 downto 0) select
--         processor_output <= memory_input when "000",
--                             b            when "010",
--                             a            when others;
                    
--    with inst_type(2 downto 0) select
--         memory_output <= processor_input when "001",
--                             d            when "011",
--                             c            when others;
    
    with inst_type(2 downto 1) select
        write_enables <= "1111" when "00",--word
                         "1100" when "01",-- half word
                         "1000" when others;--byte
    
   
   
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
    write_enables : in std_logic_vector(3 downto 0);
    wr_data : IN  std_logic_vector(31 downto 0);
    addr : IN std_logic_vector(7 downto 0); 
    write_enable : IN std_logic;
    read_enable : IN std_logic;
    push_button : IN std_logic;    
    clock : IN std_logic;    
    rd_data : OUT std_logic_vector(31 downto 0) 
        
    );
end memory;
--00 for word 01 for hLF WORD 10 FOR BYTE
architecture data_memory of memory is
type memory_array is array(0 to 255) of std_logic_vector(31 downto 0);
signal data_memory : memory_array ;
signal address : Integer := 0; 
--component BRAM_wrapper is
-- port (
--   BRAM_PORTA_addr : in STD_LOGIC_VECTOR ( 12 downto 0 );
--   BRAM_PORTA_clk : in STD_LOGIC;
--   BRAM_PORTA_din : in STD_LOGIC_VECTOR ( 31 downto 0 );
--   BRAM_PORTA_dout : out STD_LOGIC_VECTOR ( 31 downto 0 );
--   BRAM_PORTA_en : in STD_LOGIC;
--   BRAM_PORTA_we : in STD_LOGIC_VECTOR ( 3 downto 0 )
-- );
-- end component;
 
--signal address : std_logic_vector(12 downto 0) := (others => '0');
begin
    address <= to_integer(unsigned(addr));
    process(clock,push_button)
        begin
        -- mov r0, #1      11100011101000000000000000000001
        -- mov r1, #1      11100011101000000001000000000001
        --add r2, r1, r0   11100000100000000010000000000001
        --cmp r0, #1       11100011010100000000000000000001
        -- moveq r1, #1    00000011101000000001000000000001
        --mul r2, r1, r0   11100000000000100000000110010000
        -- mla r3, r1, r0, r2 11100000001000110010000010010001
        -- cmp r0, r1      11100001010100000000000000000001
        -- add r3. r2, r1 lsl r0 11100000100000100011000000010001
        -- add r3. r2, r1 lsl #2 11100000100000100011000100000001
        -- str r2,[r1 r0]  11100111100000010010000000000000
        -- ldr r3,[r1 r0]  11100111100100010011000000000000
        
        --branching
--        data_memory(0) <= "11100011101000000000000000000001";
--        data_memory(1) <= "11101010000000000000000000000011";
--        data_memory(2) <= "11100011101000000001000000000001";
--        data_memory(3) <= "11100011101000000010000000000001";
--        data_memory(4) <= "11100011101000000011000000000001";
--        data_memory(5) <= "11100011101000000100000000000001";
--        data_memory(6) <= "11100011101000000101000000000001";
--        data_memory(7) <= "11100011101000000110000000000001";
        if push_button = '1' then
            data_memory(0) <= x"E3A00040";
            data_memory(1) <= x"E7913000";
            data_memory(2) <= x"E3A00080";
            data_memory(3) <= x"E7813000";
            data_memory(4) <= x"E1A08003";
            
            data_memory(5) <= x"E358000A";
            data_memory(6) <= x"BA000002";
            data_memory(7) <= x"E248800A";
            data_memory(8) <= x"E2899001";
            data_memory(9) <= x"EAFFFFFA";
            
            data_memory(10) <= x"E0887008";
            data_memory(11) <= x"E359000A";
            data_memory(12) <= x"BA000002";
            data_memory(13) <= x"E249900A";
            data_memory(14) <= x"E28AA001";
            data_memory(15) <= x"EAFFFFFA";
            
            data_memory(16) <= x"E0877209";
            data_memory(17) <= x"E35A000A";
            data_memory(18) <= x"BA000002";
            data_memory(19) <= x"E24AA00A";
            data_memory(20) <= x"E28BB001";
            data_memory(21) <= x"EAFFFFFA";
            
            data_memory(22) <= x"E087740A";
            data_memory(23) <= x"E35B000A";
            data_memory(24) <= x"BA000002";
            data_memory(25) <= x"E24BB00A";
            data_memory(26) <= x"E28CC001";
            data_memory(27) <= x"EAFFFFFA";
            
            data_memory(28) <= x"E087760B";
            data_memory(29) <= x"E3A000C0";
            data_memory(30) <= x"E7817000";

--            data_memory(0) <= x"E3A00040";
--            data_memory(1) <= x"E7913000";
--            data_memory(2) <= x"E3A00080";
--            data_memory(3) <= x"E7813000";
--            data_memory(4) <= x"E3A000C0";
--            data_memory(5) <= x"E7813000";

        end if;
       -- if read_enable = '1' then
            rd_data <= data_memory(address);
        --end if;  
        if write_enable ='1' then
                if clock'event and clock = '1' then

                    data_memory(address) <= wr_data;
                end if;
        end if;
        
    end process;
--address(5 downto 0) <= addr;
--bram: BRAM_wrapper port map
--(       BRAM_PORTA_addr => address,
--        BRAM_PORTA_clk => clock,
--        BRAM_PORTA_din => wr_data,
--        BRAM_PORTA_dout => rd_data,
--        BRAM_PORTA_en => read_enable,
--        BRAM_PORTA_we => write_enables
--); 

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
    wr_data : IN  std_logic_vector(7 downto 0);
    
    write_enable : IN std_logic;
    read_enable : IN std_logic;
    clock : IN std_logic; 
    pr : out std_logic; 

    
  rd_data : OUT std_logic_vector(7 downto 0) 
      
  );
end program_counter;

architecture program_counter of program_counter is
signal pc : std_logic_vector(7 downto 0) := "00000000";
signal count : std_logic_vector(3 downto 0) := "0000";
begin


    rd_data <= pc;
    process(clock)
        begin
        if clock'event and clock = '1' then
            if write_enable  ='1' then
            
                
--                    if count(3) = '1' then
--                        pc <= pc +1;
--                    else
--                        count <= count + 1;
--                    end if;
                
--                    if pc = "00011110" then
--                        pc <= pc;
--                        pr <= '1';
--                    else
--                   if count(3) = '1' then
--                        pc <= pc +1;
--                    else
--                        count <= count + 1;
--                    end if;
--                    end if;
                if pc = "00011110" then
                    pc <= pc;
                    pr <= '1';
                else
                    pc <= wr_data;
                end if;
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
--MUX_2_4bit
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity mux_2_4 is
port(
    data1: IN  std_logic_vector(4 downto 0);
    data2: IN  std_logic_vector(4 downto 0);  
    cntrl : IN std_logic;
  data : OUT std_logic_vector(4 downto 0)  
  );
end mux_2_4;

architecture mux of mux_2_4 is
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
    data2: IN  std_logic_vector(31 downto 0);
    data3: IN  std_logic_vector(31 downto 0);
    data4: IN  std_logic_vector(31 downto 0); 
    cntrl : IN std_logic_vector(1 downto 0);
  data : OUT std_logic_vector(31 downto 0)  
  );
end mux_4;

architecture mux of mux_4 is
--signal data2: std_logic_vector(31 downto 0):="00000000000000000000000000000100";
begin
    with cntrl select
        data <= data1 when "00",
                data2 when "01",
                data3 when "10",
                data4 when others;
end mux;

---------------------------------------------------------------------------------------------------------------
-- MUX_4_4bit
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity mux_4_4 is
port(
    data1: IN  std_logic_vector(3 downto 0);
    data2: IN  std_logic_vector(3 downto 0);
    data3: IN  std_logic_vector(3 downto 0);
    data4: IN  std_logic_vector(3 downto 0); 
    cntrl : IN std_logic_vector(1 downto 0);
  data : OUT std_logic_vector(3 downto 0)  
  );
end mux_4_4;

architecture mux of mux_4_4 is
--signal data2: std_logic_vector(31 downto 0):="00000000000000000000000000000100";
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
    --clock : IN std_logic;  
  rd : OUT std_logic_vector(31 downto 0)  
  );
end register_31;

architecture register_31 of register_31 is
signal data : std_logic_vector(31 downto 0) := (others => '0');
begin
    rd <= data;
    process(wr,enable)
        begin
        --if clock'event and clock = '1' then
        if data = wr then
            
        else
        if enable  ='1' then
                data <= wr;
        end if;
        end if;

        --end if;
    end process;

end register_31;

---------------------------------------------------------------------------------------------------------------
-- REGISTER 4 bit
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity register_3 is
port(
    wr : IN  std_logic_vector(3 downto 0);
    enable: IN std_logic; 
    --clock : IN std_logic;   
    rd : OUT std_logic_vector(3 downto 0)  
    );
end register_3;

architecture register_3 of register_3 is
signal data : std_logic_vector(3 downto 0):= "0000";
begin
    rd <= data;
    process(wr,enable)
    begin
        --if clock'event and clock = '1' then
        if enable ='1' then
                data <= wr;
        end if;
        --end if;
    end process;

end register_3;

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
signal d : std_logic_vector(23 downto 0) := "000000000000000000000001";
signal d1 : std_logic_vector(23 downto 0) := "000000000000000000000000";


begin   
    d1 <= data1 + d;
    generate1:
        for i in 0 to 23 generate
            data(i) <= d1(i);
        end generate generate1;
    generate2:
        for i in 24 to 31 generate
                data(i) <= d1(23);
        end generate generate2;
--    data(0) <= '0';
--    data(1) <= '0';
--    with data1(23) select 
--        data <= data1 + d when '0',
--                data1 + d when others;
    
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
  clock        : in std_logic;
  push_button : IN std_logic;
  reset        : in std_logic;
  pr : out std_logic; 
  ins          : out std_logic_vector(31 downto 0);
  reg          : out std_logic_vector(31 downto 0);
  F            : out std_logic_vector(3 downto 0);
  PW           : in  std_logic;
  IorD         : in  std_logic;
  MR           : in  std_logic;
  MW           : in  std_logic;
  IW           : in  std_logic;
  DW           : in  std_logic;
  Rsrc1        : in  std_logic; --control signal for operand B
  Rsrc2        : in  std_logic; --control signal for operand A
  Shi          : in  std_logic; --shift from register or immediate
  Shift        : in  std_logic; --shiftamount( from register '1' and from intruction '0')
  Shift_amount : in std_logic_vector(4 downto 0);
  Wsrc         : in  std_logic_vector(1 downto 0);
  M2R          : in  std_logic_vector(1 downto 0); -- selects REW , Bout or 
  RW           : in  std_logic;
  AW           : in  std_logic;
  XW           : in  std_logic;
  BW           : in  std_logic;
  YW           : in  std_logic;
  aluW           : in  std_logic;
  mulW           : in  std_logic;
  shftW           : in  std_logic;
  BorS           : in  std_logic_vector(1 downto 0); --selects directly B or shifted B
  Asrc1        : in  std_logic_vector(1 downto 0);
  Asrc2        : in  std_logic_vector(1 downto 0);
  MorA        : in  std_logic;
  Fset         : in  std_logic;
  alu_op       : in  std_logic_vector(3 downto 0);
  p_m_path_op  : in   std_logic_vector(3 downto 0);
  byte_offset  : in std_logic_vector(1 downto 0);
  ReW          : in  std_logic;
  HWDATA       : out std_logic_vector(31 downto 0);
  HADDR       : out std_logic_vector(15 downto 0);
  HRDATA       : in std_logic_vector(31 downto 0);
  shift_type : IN std_logic_vector(1 downto 0);  
  shift_enable : IN std_logic    
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
        shift_enable : IN std_logic;
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
        write_enables : OUT std_logic_vector(3 downto 0)     
  );
end component;

component register_file is
port(
        wr_data : IN  std_logic_vector(31 downto 0);
        wr_addr : IN std_logic_vector(3 downto 0); 
        rd_addr1 : IN std_logic_vector(3 downto 0); 
        rd_addr2 : IN std_logic_vector(3 downto 0); 
        write_enable : IN std_logic;
       -- clock : IN std_logic;
       push_button : IN std_logic;
        reset : IN std_logic; 
        rr : OUT std_logic_vector(31 downto 0) ; 
        rd_data1 : OUT std_logic_vector(31 downto 0) ; 
        rd_data2 : OUT std_logic_vector(31 downto 0)     
  );
end component;

component memory is
port(
        write_enables : in std_logic_vector(3 downto 0);
        wr_data : IN  std_logic_vector(31 downto 0);
        addr : IN std_logic_vector(7 downto 0); 
        write_enable : IN std_logic;
        read_enable : IN std_logic;
        push_button : IN std_logic;
        clock : IN std_logic;    
        rd_data : OUT std_logic_vector(31 downto 0)  
  );
end component;

component program_counter is
port(
        wr_data : IN  std_logic_vector(7 downto 0);
        write_enable : IN std_logic;
        read_enable : IN std_logic;
        clock : IN std_logic; 
        pr : out std_logic; 
        rd_data : OUT std_logic_vector(7 downto 0) 
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

component mux_2_4 is
port(
        data1: IN  std_logic_vector(4 downto 0);
        data2: IN  std_logic_vector(4 downto 0);  
        cntrl : IN std_logic;
        data : OUT std_logic_vector(4 downto 0)  
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
        data2: IN  std_logic_vector(31 downto 0);
        data3: IN  std_logic_vector(31 downto 0);
        data4: IN  std_logic_vector(31 downto 0); 
        cntrl : IN std_logic_vector(1 downto 0);
        data : OUT std_logic_vector(31 downto 0)  
  );
end component;

component mux_4_4 is
port(
        data1: IN  std_logic_vector(3 downto 0);
        data2: IN  std_logic_vector(3 downto 0);
        data3: IN  std_logic_vector(3 downto 0);
        data4: IN  std_logic_vector(3 downto 0); 
        cntrl : IN std_logic_vector(1 downto 0);
        data : OUT std_logic_vector(3 downto 0)  
  );
end component;

component register_31 is
port(
        wr : IN  std_logic_vector(31 downto 0);
        enable: IN std_logic;
     --   clock : IN std_logic; 
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

component register_3 is
port(
        wr : IN  std_logic_vector(3 downto 0);
        enable: IN std_logic;
     --   clock : IN std_logic;    
        rd : OUT std_logic_vector(3 downto 0)  
    );
end component;



signal pc_output : std_logic_vector(7 downto 0) := (others => '0') ;
signal memory_input_addr : std_logic_vector(7 downto 0) := (others => '0') ;
signal res_output : std_logic_vector(31 downto 0) := (others => '0') ;
signal memory_input_data : std_logic_vector(31 downto 0) := (others => '0') ; --output for pm path
signal memory_input_data1 : std_logic_vector(31 downto 0) := (others => '0') ; --output for pm path
signal memory_output_data : std_logic_vector(31 downto 0) := (others => '0') ; -- direct output from memory
signal instruction : std_logic_vector(31 downto 0) := (others => '0') ;
signal dr_output : std_logic_vector(31 downto 0) := (others => '0') ; --input from pm path
signal mem_out : std_logic_vector(31 downto 0) := (others => '0') ; --output from pm path
signal rd_addr1 : std_logic_vector(3 downto 0) := (others => '0') ;
signal rd_addr2 : std_logic_vector(3 downto 0) := (others => '0') ;
signal rd_addr2_1 : std_logic_vector(3 downto 0) := (others => '0') ;
signal wr_addr : std_logic_vector(3 downto 0) := (others => '0') ;
signal immediate_operand : std_logic_vector(11 downto 0) := (others => '0') ;
signal immediate_operand_DP : std_logic_vector(31 downto 0) := (others => '0') ;
signal branch_offset : std_logic_vector(23 downto 0) := (others => '0') ;
signal branch_offset_ext : std_logic_vector(31 downto 0) := (others => '0') ;
signal operand_ext: std_logic_vector(31 downto 0) := (others => '0') ;
signal wr_data : std_logic_vector(31 downto 0) := (others => '0') ;
signal rd_data1 : std_logic_vector(31 downto 0) := (others => '0') ;
signal rd_data2 : std_logic_vector(31 downto 0) := (others => '0') ;
signal A_out : std_logic_vector(31 downto 0) := (others => '0') ;
signal B_out : std_logic_vector(31 downto 0) := (others => '0') ; --input for pm path
signal Y_out : std_logic_vector(31 downto 0) := (others => '0') ;
signal B_out_1 : std_logic_vector(31 downto 0) := (others => '0') ;
signal X_out : std_logic_vector(31 downto 0) := (others => '0') ;
signal shft_out : std_logic_vector(31 downto 0) := (others => '0') ;--output for shift register
signal shft_in : std_logic_vector(31 downto 0) := (others => '0') ;--input for shift register
signal mul_out : std_logic_vector(31 downto 0) := (others => '0') ;--output for mul register
signal mul_in : std_logic_vector(31 downto 0) := (others => '0') ;--input for mul register
signal alu_out : std_logic_vector(31 downto 0) := (others => '0') ;--output for alu register
signal alu_in : std_logic_vector(31 downto 0) := (others => '0') ;--input for alu register
signal alu_input1 : std_logic_vector(31 downto 0) := (others => '0') ;
signal alu_input2 : std_logic_vector(31 downto 0) := (others => '0') ;
signal shft_input : std_logic_vector(31 downto 0) := (others => '0') ;--input for shifter_unit
signal data_addr : std_logic_vector(31 downto 0) := (others => '0') ;
signal flagsd : std_logic_vector(3 downto 0) := (others => '0') ;
signal fla : std_logic := '0' ;
signal MR_new : std_logic  ;
signal MW_new : std_logic ;
signal flags : std_logic_vector(3 downto 0) := (others => '0') ;
signal F1 : std_logic_vector(3 downto 0) := (others => '0') ;
signal write_enable_for_memory : std_logic_vector(3 downto 0) := (others => '0') ;
signal pc_extended : std_logic_vector(31 downto 0) := (others => '0') ;
signal output_asrc1 : std_logic_vector(31 downto 0) := (others => '0') ;
signal output_asrc2 : std_logic_vector(31 downto 0) := (others => '0') ;
signal final_output : std_logic_vector(31 downto 0) := (others => '0') ;
signal shft_amount : std_logic_vector(4 downto 0) := (others => '0') ;
--signal shi : std_logic ;

begin
branch_offset <= instruction(23 downto 0);
immediate_operand <= instruction(11 downto 0);
immediate_operand_DP(7 downto 0) <= instruction(7 downto 0);
pc_extended(7 downto 0) <= pc_output(7 downto 0);
--shi <= '1';


pc : program_counter port map
(   wr_data => alu_in(7 downto 0),
    write_enable => PW,
    read_enable => '1',
    clock => clock,
    pr   => pr,
    rd_data => pc_output
);

--mux aftet program counter register to fetch instrucyion or write in memory
--IorD_mux : mux_2_5 port map  
--(   data1 => pc_output,
--    data2 => res_output(5 downto 0),
--    cntrl => IorD,
--    data => memory_input_addr
--);

memory_input_addr <= pc_output;
HADDR <= res_output(15 downto 0);

MW_new <= MW and (not IorD);
MR_new <= MR and (not IorD);

memory_unit : memory port map
(       write_enables => write_enable_for_memory ,
        wr_data => memory_input_data1,
        addr => memory_input_addr, 
        write_enable => MW_new,
        read_enable => MR_new,
        push_button => push_button,
        clock => clock, 
        rd_data => memory_output_data

);

IR : register_31 port map
(   wr => memory_output_data,
    enable => IW,
   -- clock => clock,
    rd => instruction
);
ins <= instruction;

DR : register_31 port map
(   wr => HRDATA,
    enable => DW,
   -- clock => clock,
    rd => dr_output
);

HWDATA <= memory_input_data;

Mul_read_address1_in_file : mux_2_3 port map
(   data1 => instruction(19 downto 16),--rn
    data2 => instruction(11 downto 8),--rs
    cntrl => Rsrc2,
    data => rd_addr1
);

Mul_read_address2_in_file : mux_2_3 port map
(   data1 => instruction(3 downto 0),--rm
    data2 => instruction(15 downto 12),--rd
    cntrl => Rsrc1,
    data => rd_addr2
); 
 
Mul_write_address_in_file : mux_4_4 port map
(   data1 => instruction(15 downto 12),--rd
   data2 => instruction(19 downto 16),--rn
   data3 => "1110",
   data4 => "1110",
   cntrl => Wsrc,
   data => wr_addr
);   

Mul_write_data_in_file : mux_4 port map
(  data1 => pc_extended,
   data2 => res_output,
   data3 => mem_out,
   data4 => "00000000000000000000000000000000",    
   cntrl => M2R,
   data =>  wr_data
); 
--dr_output -> mem_out
Processor_memory_path : p_m_path port map
(   
    processor_input => B_out,
    memory_input => HRDATA, 
    inst_type => p_m_path_op, 
    byte_offset =>  byte_offset,  
    processor_output => mem_out,
    memory_output => memory_input_data,
    write_enables => write_enable_for_memory    
);

register_file_unit : register_file port map
(       wr_data => wr_data,
        wr_addr => wr_addr,
        rd_addr1 => rd_addr1,
        rd_addr2 => rd_addr2,
        write_enable => RW,
       -- clock => clock,
        push_button => push_button,
        reset => reset,
        rr => reg, 
        rd_data1 => rd_data1, 
        rd_data2 => rd_data2 
    
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


--register for operand A after registerfile
A : register_31 port map
(   wr => rd_data1,
    enable => AW,
   -- clock => clock,
    rd => A_out
);
--register for operand X(shift amount) after registerfile
X : register_31 port map
(   wr => rd_data1,
    enable => XW,
  --  clock => clock,
    rd => X_out
);
--register for operand B after registerfile
B : register_31 port map
(   wr => rd_data2,
    enable => BW,
  --  clock => clock,
    rd => B_out
);
--register for operand B after registerfile for mla instruction
Y : register_31 port map
(   wr => rd_data2,
    enable => YW,
  --  clock => clock,
    rd => Y_out
);

--register after shft
shift_register : register_31 port map
(   wr => shft_in,
    enable => shftW,
  --  clock => clock,
    rd => shft_out
);
--register after mul
mul_register : register_31 port map
(   wr => mul_in,
    enable => mulW,
  --  clock => clock,
    rd => mul_out
);
--register after alu
alu_register : register_31 port map
(   wr => alu_in,
    enable => aluW,
 --   clock => clock,
    rd => alu_out
);
--multiplexer for alu_input1  
Mul_for_asrc1 : mux_4 port map
(   data1 => A_out,
    data2 => mul_out,
    data3 => pc_extended,
    data4 => "00000000000000000000000000000000",
    cntrl => Asrc1,
    data => alu_input1
); 
--multiplexer for choosing b directly or shifted B
Mul_for_B : mux_4 port map
(   data1 => B_out,
    data2 => shft_out,
    data3 => Y_out,
    data4 => Y_out,
    cntrl => BorS,
    data => B_out_1
); 

--multiplexer for alu_input2
Mul_for_asrc2 : mux_4 port map
(  data1 => B_out_1,
   data2 => "00000000000000000000000000000001",
   data3 => operand_ext,
   data4 => branch_offset_ext,    
   cntrl => Asrc2,
   data =>  alu_input2
); 
--multiplexer for shift_input1
Mul_for_shifter : mux_2_31 port map
(   data1 => immediate_operand_DP,
    data2 => B_out,
    cntrl => shi,
    data => shft_input
); 

--multiplexer for shift_amount
Mul_for_shiftamount : mux_2_4 port map
(   data1 => X_out(4 downto 0),
    data2 => shift_amount,
    cntrl => shift,
    data => shft_amount
);
--with shift select 
--    shft_amount <= X_out when '0',
--                    shift_amount when others;
       

shifter_unit : shifter port map
(
    op1 => shft_input,
    shift_type => shift_type, 
    shift_amount => shft_amount,       
    shift_enable => shift_enable,
    shift_carry => fla,  
    result => shft_in
  );
  
multiplier_unit : multiplier port map
       (    op1 => A_out,
            op2 => B_out,    
            result => mul_in,
            flags => flagsd        
        );


alu_unit : alu port map
( op1 => alu_input1,
  op2 => alu_input2, 
  result => alu_in,
  flags => flags,
  operation => alu_op,
  carry => F1(1)  
);

Mux_for_final_output : mux_2_31 port map
(   data1 => mul_out,
    data2 => alu_out,
    cntrl => MorA,
    data => final_output
);


RES : register_31 port map
(   wr =>final_output,
    enable => ReW,
 --   clock => clock,
    rd => res_output
);

Flag : register_3 port map
(   wr => flags,
    enable => Fset,
  --  clock => clock,    
    rd => F1  
);
F <= F1;
--with Fset select
--    F <= flags when '1',
--         "0000" when others;

--with Fset select
--    F <= flags when '1',
--         "0000" when others;

end datapath;











