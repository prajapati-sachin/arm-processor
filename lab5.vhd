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

---------------------------------------------------------------------------------------------------------------
--INSTRUCTION DECODER
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity Instr_decoder is
port(
    Instruction: IN  std_logic_vector(31 downto 0); 
    F : OUT std_logic_vector(1 downto 0);  
    DP_imm: OUT std_logic;
    no_result: OUT std_logic;
    INVALID: OUT std_logic;
    immediate: OUT std_logic_vector(7 downto 0);
    alu_op: OUT std_logic_vector(3 downto 0);
    ShTyp : OUT std_logic_vector(3 downto 0);
    Sh_amount : OUT std_logic_vector(7 downto 0);
    mul: OUT std_logic;
    mla: OUT std_logic;
    Sh_imm : OUT std_logic;
    S : OUT std_logic  
    );
end Instr_decoder;
<<<<<<< HEAD
=======

architecture Instr_decoder of Instr_decoder is
begin
   process(Instruction)
        begin
        case Instruction(27 downto 26) is
            when "00" =>  F <= "00"; 
                          ----------------------------------------------------------------------
                          --DP immediate
                          -- Operand is immediate
                          if Instruction(25) = '1' then
                               DP_imm <= '1' ;
                               alu_op  <= Instruction(24 downto 21);
                               --if instruction is of cmp, tst type 
                               if Instruction(24 downto 23) = "10" then 
                                    no_result <= '1';
                               else
                                    no_result <= '0';
                               end if;
                               immediate <= Instruction(7 downto 0);
                               ShTyp <= "11";
                               Sh_amount <= Instruction(11 downto 8) + Instruction(11 downto 8);  
                               Sh_imm <= '1';
                          ----------------------------------------------------
                          else
                            ------------------------------------------------------------------------
                            -- DP ShAmt imm
                            if Instruction(4) <= '0' then
                                DP_imm <= '0';
                                alu_op <= Instruction(24 downto 21);
                                 --if instruction is of cmp, tst type 
                                  if Instruction(24 downto 23) = "10" then 
                                       no_result <= '1';
                                  else
                                       no_result <= '0';
                                  end if;
                                  ShTyp <= Instruction(6 downto 5);
                                  Sh_amount <= Instruction(11 downto 7);
                                  Sh_imm <= '1';     
                             ------------------------------------------------------------------------
                              
                            else
                                if Instruction(7) <= '0' then 
                                    --Instruction in invalid
                                    if instruction(11 downto 8)= "1111" then
                                        Invalid <= '1';
                                    else   
                                       -- DP ShAmt register
                                        DP_imm <= '0';
                                        alu_op <= Instruction(24 downto 21);
                                        --if instruction is of cmp, tst type 
                                        if Instruction(24 downto 23) = "10" then 
                                             no_result <= '1';
                                        else
                                             no_result <= '0';
                                        end if;
                                        ShTyp <= Instruction(6 downto 5);
                                        Sh_amount <= Instruction(11 downto 8);
                                        Sh_imm <= '0';
                                    end if;
                                --------------------------------------------------------
                                --Instruction(7) is 1 for MUL and MLA
                                else                                               
                                    if Instruction(6 downto 5) = "00" then
                                        if Instruction(24 downto 23) = "00" then
                                            if Instruction(21) = '0' then
                                                mul <= '1';
                                            else
                                                mla <= '1';
                                            end if;
                                        end if;
                                    else
                                        
                                                
                               end if;
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          end if;
            when "01" =>
            
            when "10" =>
            
            when others => 
                
        end case;
   end process;
end Instr_decoder;


>>>>>>> 2b74420e6f0d210812e257a1d8d20d3c25a4b485

architecture Instr_decoder of Instr_decoder is
begin
   process(Instruction)
        begin
        case Instruction(27 downto 26) is
            when "00" =>  F <= "00"; 
                          ----------------------------------------------------------------------
                          --DP immediate
                          -- Operand is immediate
                          if Instruction(25) = '1' then
                               DP_imm <= '1' ;
                               alu_op  <= Instruction(24 downto 21);
                               --if instruction is of cmp, tst type 
                               if Instruction(24 downto 23) = "10" then 
                                    no_result <= '1';
                               else
                                    no_result <= '0';
                               end if;
                               immediate <= Instruction(7 downto 0);
                               ShTyp <= "11";
                               Sh_amount <= Instruction(11 downto 8) + Instruction(11 downto 8);  
                               Sh_imm <= '1';
                          ----------------------------------------------------
                          else
                            ------------------------------------------------------------------------
                            -- DP ShAmt imm
                            if Instruction(4) <= '0' then
                                DP_imm <= '0';
                                alu_op <= Instruction(24 downto 21);
                                 --if instruction is of cmp, tst type 
                                  if Instruction(24 downto 23) = "10" then 
                                       no_result <= '1';
                                  else
                                       no_result <= '0';
                                  end if;
                                  ShTyp <= Instruction(6 downto 5);
                                  Sh_amount <= Instruction(11 downto 7);
                                  Sh_imm <= '1';     
                             ------------------------------------------------------------------------
                              
                            else
                                if Instruction(7) <= '0' then 
                                    --Instruction in invalid
                                    if instruction(11 downto 8)= "1111" then
                                        Invalid <= '1';
                                    else   
                                       -- DP ShAmt register
                                        DP_imm <= '0';
                                        alu_op <= Instruction(24 downto 21);
                                        --if instruction is of cmp, tst type 
                                        if Instruction(24 downto 23) = "10" then 
                                             no_result <= '1';
                                        else
                                             no_result <= '0';
                                        end if;
                                        ShTyp <= Instruction(6 downto 5);
                                        Sh_amount <= Instruction(11 downto 8);
                                        Sh_imm <= '0';
                                    end if;
                                --------------------------------------------------------
                                --Instruction(7) is 1 for MUL and MLA
                                else                                               
                                   if 
                                   DP_imm <= '0';
                                   alu_op <= Instruction(24 downto 21);
                                   --if instruction is of cmp, tst type 
                                   if Instruction(24 downto 23) = "10" then 
                                        no_result <= '1';
                                   else
                                        no_result <= '0';
                                   end if;
                                   ShTyp <= Instruction(6 downto 5);
                                   Sh_amount <= Instruction(11 downto 8);
                                   Sh_imm <= '0';
                               end if;
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          end if;
            when "01" =>
            
            when "10" =>
            
            when others => 
                
        end case;
   end process;
end Instr_decoder;