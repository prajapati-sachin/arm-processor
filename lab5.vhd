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
    F : IN  std_logic_vector(1 downto 0);  
    DP_op: IN  std_logic_vector(3 downto 0);    
    DT_U: IN  std_logic;
    alu_op : OUT std_logic_vector(3 downto 0)
    );
end Actrl;

architecture Actrl of Actrl is
signal dp : std_logic;
signal dt : std_logic;
signal br : std_logic;
signal mla : std_logic;
begin
    process(DP_op,DT_U,F)
    begin
        if F = "00" then
            alu_op <= DP_op;
        elsif F = "01" then 
            if DT_U = '1' then
                alu_op <= "0100";
            else
                alu_op <= "0010";
            end if;
        elsif F="10" then 
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
    alu_e: OUT std_logic;
    mla_e: OUT std_logic;
    Sh_imm : OUT std_logic;
    DT_reg : OUT std_logic;
    DT_post: OUT std_logic;
    DT_Byte: OUT std_logic;
    DT_Writeback: OUT std_logic;
    DT_Load : OUT std_logic;
    DT_U : OUT std_logic;
    DT_immediate : OUT std_logic_vector(11 downto 0);
    B : OUT std_logic;
    S : OUT std_logic  
    );
end Instr_decoder;

architecture Instr_decoder of Instr_decoder is
begin
   process(Instruction)
        begin
        case Instruction(27 downto 26) is
            when "00" =>  F <= "00"; 
                          S<=Instruction(20);
                          ----------------------------------------------------------------------
                          --DP immediate
                          -- Operand is immediate
                          if Instruction(25) = '1' then
                               alu_e <= '1';
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
                                alu_e <= '1';
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
                                        alu_e <= '1';
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
                                                alu_e <= '0';
                                                mla_e <= '0';
                                            else
                                                alu_e <= '0';
                                                mla_e <= '1';
                                            end if;
                                        end if;
                                    else 
                                    -------------------------------------
                                    -- halfword DT
                                    if Instruction(22) = '0' then
                                        --register
                                    else
                                        --immediate  
                                    end if;            
                               end if;
                          
                          
                          
                            end if;
                          
                          
                          
                          
                          
                          
                          end if;
                          end if;
            when "01" =>  F <= "01";
                          DT_reg <= Instruction(25);
                          if Instruction(25) = '0' then
                            DT_immediate <= Instruction(11 downto 0);
                          else
                            ShTyp <= Instruction(6 downto 5);
                            Sh_amount <= Instruction(11 downto 7); 
                          end if;
                          DT_post <= Instruction(24);
                          DT_U <= Instruction(23);
                          DT_Byte <= Instruction(22);
                          DT_Writeback <= Instruction(21);
                          DT_Load <= Instruction(20);
                          
            
            when "10" =>  F<="10";
                          B<= '1';
                          alu_op <= "0100";
            
            when others => INVALID <= '1';
                
        end case;
   end process;
end Instr_decoder;


---------------------------------------------------------------------------------------------------------------
--Controller_FSM
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity main_controller is
port(
        clock: IN std_logic;
        F : In std_logic_vector(1 downto 0);  
        DP_imm: In std_logic;
        no_result: In std_logic;
        INVALID: In std_logic;
        immediate: In std_logic_vector(7 downto 0);
        alu_op: In std_logic_vector(3 downto 0);
        ShTyp : In std_logic_vector(3 downto 0);
        Sh_amount : In std_logic_vector(7 downto 0);
        alu_e: IN std_logic;
        mla_e: In std_logic;
        Sh_imm : In std_logic;
        DT_reg : In std_logic;
        DT_post: In std_logic;
        DT_Byte: In std_logic;
        DT_Writeback: In std_logic;
        DT_Load : In std_logic;
        DT_U : In std_logic;
        DT_immediate : In std_logic_vector(11 downto 0);
        B : In std_logic;
        S : In std_logic  

    );
end main_controller;

-- DP instr       fetch -> rdAB -> rdrs     ->  shft        ->arith       -> wrRF 
-- DP Mul         fetch -> rdAB -> rdrs     ->  mul         ->arith       -> wrRF
-- DT instr(str)  fetch -> rdAB -> shft     ->  addr(rdB)   -> wrM(wrRF) 
-- DT instr(ldr)  fetch -> rdAB -> shft     ->  addr(rdB)   -> rdM(wrRF) -> M2RF
-- B  instr       fetch -> rdAB -> brn(wrRF)


architecture main_controller of main_controller is
type State IS (     
                   fetch,
                   rdAB,
                   rdrs,
                   mul,
                   arith,
                   wrRF,--both dt and dp
                   shft,
                   addr,
                   wrM,
                   rdM,
                   M2RF,
                   brn                   
);
signal curr_state :State := fetch;
begin
    process(clock)
    begin
        if clock'event  and clock ='1' then
            if curr_state = fetch then
                curr_state <= rdAB;
            end if;
            if curr_state = rdAB then
                if F = "00" then
                    if alu_e = '1' then
                        if sh_imm = '0' then
                            curr_state <= rdrs;
                        else
                            curr_state <= arith;
                        end if;
                    else
                        if mla_e = '1' then
                            curr_state <= rdrs;
                        else 
                            curr_state <= mul;
                        end if;
                    end if;
                elsif F = "01" then
                    curr_state <= shft;
                elsif F = "10" then
                    curr_state <= brn;
                end if;
            end if;
            if curr_state = rdrs then
                if F = "00" then
                    curr_state <= arith;
                else 
                    curr_state <= mul;
                end if;
            end if;
            if curr_state = arith then
                curr_state <= wrRF;
            end if;
            if curr_state = mul then
                if mla_e = '0' then 
                    curr_state <= wrRF;
                else 
                    curr_state <= arith;
                end if;
            end if;            
            if curr_state = wrRF then
                curr_state <= fetch;
            end if;
            if curr_state = shft then
                curr_state <= addr;
            end if;
            if curr_state = addr then
                if DT_load = '0' then
                    curr_state <= wrM;
                else 
                    curr_state <= rdM;
                end if;
            end if;
            if curr_state = wrM then
                curr_state <= wrRF;
            end if;
            if curr_state = rdM then
                curr_state <= M2RF;
            end if;
            if curr_state = M2RF then
                curr_state <= wrRF;
            end if;
            if curr_state = brn then
                curr_state <= fetch;
            end if;
        end if;
    end process;
    
    process(curr_state,F,DP_imm,no_result,INVALID,immediate,alu_op,ShTyp,Sh_amount,alu_e,mla_e,Sh_imm ,DT_reg,DT_post,DT_Byte,DT_Writeback,DT_Load ,DT_U ,DT_immediate ,B ,S  )
    begin
        
    end process;      
end main_controller;

