---------------------------------------------------------------------------------------------------------------
--BCtrl
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity Bctrl is
port(
    flags: IN  std_logic_vector(3 downto 0);    
    instr: IN  std_logic_vector(3 downto 0);
    p : OUT std_logic 
    );
end Bctrl;

architecture Bctrl of Bctrl is
signal z : std_logic;
signal c : std_logic;
signal n : std_logic;
signal v : std_logic;
begin
--Flag order = N Z C V
             --Flag(0)-V
             --Flag(1)-C
             --Flag(2)-Z
             --Flag(3)-N
n <= flags(3);
z <= flags(2);
c <= flags(1);
v <= flags(0);

with instr select 
    p <= z                         when "0000",
         not z                     when "0001",
         c                         when "0010",
         not c                     when "0011",
         n                         when "0100",
         not n                     when "0101",
         v                         when "0110",
         not v                     when "0111",
         (not z) and c             when "1000",
         not((not z) and c)        when "1001",
         not(n xor v)              when "1010",
         n xor v                   when "1011",
         not(z or (n xor v))       when "1100",
         z or (n xor v)            when "1101",
         '1'                       when others;
    
end Bctrl;


---------------------------------------------------------------------------------------------------------------
--ACtrl
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity Actrl is
port(
    state: IN  std_logic_vector(3 downto 0);    
    instr: IN  std_logic_vector(3 downto 0);
    alu_op : OUT std_logic_vector(3 downto 0)
    );
end Actrl;

architecture Actrl of Actrl is
signal dp : std_logic;
signal dt : std_logic;
signal br : std_logic;
signal mla : std_logic;
begin
    process(instr,state)
    begin
        if dp = '1' then
            alu_op <= instr;
        elsif dt = '1' then 
            if instr(23) = '1' then
                alu_op <= "0100";
            else
                alu_op <= "0010";
            end if;
        elsif br = '1' then 
            alu_op <= "0100";
        end if;
    end process;
    
end Actrl;




