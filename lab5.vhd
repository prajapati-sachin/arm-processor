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
    Sh_amount : OUT std_logic_vector(4 downto 0);
    alu_e: OUT std_logic;
    mla_e: OUT std_logic;
    Sh_imm : OUT std_logic;
    DT_reg : OUT std_logic;
    DT_post: OUT std_logic;
    DT_Byte: OUT std_logic;
    DT_Half: OUT std_logic;
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
                                        DT_reg <= '1';
                                        DT_post <= Instruction(24);
                                        DT_U <= Instruction(23);
                                        DT_Half <= '1';
                                        DT_Writeback <= Instruction(21);
                                        DT_Load <= Instruction(20);
                                    else
                                        --immediate  
                                        DT_reg <= '0';
                                        DT_post <= Instruction(24);
                                        DT_U <= Instruction(23);
                                        DT_Half <= '1';
                                        DT_Writeback <= Instruction(21);
                                        DT_Load <= Instruction(20);
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
                            sh_imm <= '1';
                          end if;
                          DT_post <= Instruction(24);
                          DT_U <= Instruction(23);
                          DT_Byte <= Instruction(22);
                          DT_Writeback <= Instruction(21);
                          DT_Load <= Instruction(20);
                          
            
            when "10" =>  F<="10";
                          B<= Instruction(24);;
                          --alu_op <= "0100";
            
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
 --       F : In std_logic_vector(1 downto 0);  
        reset : out std_logic;
        ins          : in std_logic_vector(31 downto 0);
        F            : in std_logic_vector(3 downto 0);
        PW           : out  std_logic;
        IorD         : out  std_logic;
        MR           : out  std_logic;
        MW           : out  std_logic;
        IW           : out  std_logic;
        DW           : out  std_logic;
        Rsrc1        : out  std_logic; --control signal for operand B
        Rsrc2        : out  std_logic; --control signal for operand A
        Shi          : out  std_logic; --shift from register or immediate
        Shift        : out  std_logic; --shiftamount( from register '1' and from intruction '0')
        Shift_amount : out std_logic_vector(7 downto 0);
        Wsrc         : out  std_logic_vector(1 downto 0);
        M2R          : out  std_logic_vector(1 downto 0); -- selects REW , Bout or 
        RW           : out  std_logic;
        AW           : out  std_logic;
        XW           : out  std_logic;
        BW           : out  std_logic;
        aluW           : out  std_logic;
        mulW           : out  std_logic;
        shftW           : out  std_logic;
        BorS           : out  std_logic; --selects directly B or shifted B
        Asrc1        : out  std_logic_vector(1 downto 0);
        Asrc2        : out  std_logic_vector(1 downto 0);
        MorA        : out  std_logic;
        Fset         : out  std_logic;
        alu_op       : out  std_logic_vector(3 downto 0);
        p_m_path_op  : out   std_logic_vector(3 downto 0);
        byte_offset  : out std_logic_vector(1 downto 0);
        ReW          : out  std_logic;
        shift_type : out std_logic_vector(1 downto 0);  
        shift_enable : out std_logic  

    );
end main_controller;

-- DP instr       fetch -> rdAB -> rdrs     ->  shft        ->arith       -> wrRF 
-- DP Mul         fetch -> rdAB -> rdrs     ->  mul         ->arith       -> wrRF
-- DT instr(str)  fetch -> rdAB -> shft     ->  addr(rdB)   -> wrM(wrRF) 
-- DT instr(ldr)  fetch -> rdAB -> shft     ->  addr(rdB)   -> rdM(wrRF) -> M2RF
-- B  instr       fetch -> rdAB -> brn(wrRF)


architecture main_controller of main_controller is
component Instr_decoder is
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
    DT_Half: OUT std_logic;
    DT_Writeback: OUT std_logic;
    DT_Load : OUT std_logic;
    DT_U : OUT std_logic;
    DT_immediate : OUT std_logic_vector(11 downto 0);
    B : OUT std_logic;
    S : OUT std_logic  
    );
end component;

component Bctrl is
port(
    flags: IN  std_logic_vector(3 downto 0);    
    instr: IN  std_logic_vector(3 downto 0);
    p : OUT std_logic 
    );
end component;

component Actrl is
port(
    F : IN  std_logic_vector(1 downto 0);  
    DP_op: IN  std_logic_vector(3 downto 0);    
    DT_U: IN  std_logic;
    alu_op : OUT std_logic_vector(3 downto 0)
    );
end component;

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
signal  Fo :  std_logic_vector(1 downto 0);  
signal  DP_imm:  std_logic;
signal  no_result:  std_logic;
signal  INVALID:  std_logic;
signal  immediate:  std_logic_vector(7 downto 0);
signal  alu_operation:  std_logic_vector(3 downto 0);
signal  ShTyp :  std_logic_vector(3 downto 0);
signal  Sh_amount :  std_logic_vector(7 downto 0);
signal  alu_e:  std_logic;
signal  mla_e:  std_logic;
signal  Sh_imm :  std_logic;
signal  DT_reg :  std_logic;
signal  DT_post:  std_logic;
signal  DT_Byte:  std_logic;
signal  DT_Half:  std_logic;
signal  DT_Writeback:  std_logic;
signal  DT_Load :  std_logic;
signal  DT_U :  std_logic;
signal  DT_immediate :  std_logic_vector(11 downto 0);
signal  B :  std_logic;
signal  S :  std_logic;
signal  p:  std_logic; --predicate
begin
    process(clock)
    begin
        if clock'event  and clock ='1' then
            if curr_state = fetch then
                curr_state <= rdAB;
            end if;
            if curr_state = rdAB then
                if Fo = "00" then
                    if alu_e = '1' then
                        if sh_imm = '0' then
                            curr_state <= rdrs;
                        else
                            curr_state <= shft;
                        end if;
                    else
                        if mla_e = '1' then
                            curr_state <= rdrs;
                        else 
                            curr_state <= mul;
                        end if;
                    end if;
                elsif Fo = "01" then
                    curr_state <= shft;
                elsif Fo = "10" then
                    curr_state <= brn;
                end if;
            end if;
            if curr_state = rdrs then
                if Fo = "00" then
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
                if Fo = "00" then
                    curr_state <= arith;
                else
                    curr_state <= addr;
                end if;
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
    
    instruction_decoder : Instr_decoder port map
    (
        Instruction => ins,
        F  =>  Fo,
        DP_imm => DP_imm,
        no_result => no_result,
        INVALID => INVALID,
        immediate => immediate,
        alu_op => alu_operation,
        ShTyp  => ShTyp,
        Sh_amount  => Sh_amount,
        alu_e => alu_e,
        mla_e => mla_e,
        Sh_imm  => Sh_imm,
        DT_reg  => DT_reg,
        DT_post => DT_post,
        DT_Byte => DT_Byte,
        DT_Half => DT_Half,
        DT_Writeback => DT_Writeback,
        DT_Load  => DT_Load,
        DT_U  => DT_U,
        DT_immediate  => DT_immediate  
    );
    
    Acontrol : Actrl port map
    (
        F => Fo,
        DP_op => alu_operation, 
        DT_U => DT_U,
        alu_op => alu_op
    );
    
    Bcontrol : Bctrl port map
    (
        flags => F,  
        instr => ins(31 downto 28),
        p => p
    );
    
    process(curr_state,Fo,DP_imm,no_result,INVALID,immediate,alu_operation,ShTyp,Sh_amount,alu_e,mla_e,Sh_imm ,DT_reg ,DT_post,DT_Byte,DT_Writeback,DT_Load ,DT_U ,DT_immediate,B,S,p )
    begin
        if curr_state = fetch then
            IorD <= '0';
            PW <= '1';
            IW <= '1';
            DW <= '0';
            alu_op <= "0100";
            MR <= '1';
            Asrc1 <= "10";
            Asrc2 <= "01";
        elsif curr_state = rdAB then
            AW <= '1';
            BW <= '1';
            Rsrc2 <= '0';
            Rsrc1 <= '0';
        elsif curr_state = rdrs then
            XW <= '1';
            Rsrc2 <= '1';
        elsif curr_state = shft then
            shift <= sh_imm;  --shift amount is immediate or from register
            if DP_imm = '1' then
                shi <= '0';
            else 
                shi <= '1' ;
            end if;
            if Fo = "01" then
                shi <= '1';
            end if;
            shift_type <= shTyp;
            shift_amount <= sh_amount;
            shftW <= '1';
        elsif curr_state = mul then
            mulW <= '1';
            if mla_e = '0' then
                MorA <= '0';
                ReW <= '1';
            else 
                REW <= '0';
            end if;
        elsif curr_state = arith then
            if alu_e = '1' then
                Asrc1 <= "00";
                BorS <= '1';
                Asrc2 <= "00";   
            elsif mla_e = '1' then
                Asrc1 <= "01";
                BorS <= '0';
                Asrc2 <= "00";
            end if;
            aluW <='1';
            MorA <= '1';
            ReW <= '1';
        elsif curr_state = wrRF then
            RW <= p;
            Wsrc <= "00";
            M2R <= "01";
        elsif curr_state = addr then
            Asrc1 <= "00";
            if DT_reg = '1' then
                Asrc2 <= "10";
            else
                BorS <= '1';
                Asrc2 <= "00";
            end if;
            aluW <='1';
            BW <= '1';
            Rsrc1 <= '1';
        elsif curr_state = wrM then
            MW <= p;
            if DT_half = '1' then
                p_m_path_op <= "0010";
                byte_offset <= "01";
            elsif DT_Byte = '1' then
                p_m_path_op <= "0101";
                byte_offset <= "11";
            else
                p_m_path_op <="0001";
            end if;
            --half full and byte
            if DT_Writeback = '1' then
                RW <= p;
                Wsrc <= "01";
                M2R <= "10";
            else 
                RW <= '0';
            end if;
        elsif curr_state = rdM then
            IorD <= '1';
            MR <= '1';
            IW <= '0';
            DW <= '1';
            if DT_Writeback = '1' then
                RW <= p;
                Wsrc <= "01";
                M2R <= "10";
            else 
                RW <= '0';
            end if;
        elsif curr_state = M2RF then
            RW <= p;
            if DT_half = '1' then
                p_m_path_op <= "0011";
                byte_offset <= "01";
            elsif DT_Byte = '1' then
                p_m_path_op <= "0101";
                byte_offset <= "11";
            else
                p_m_path_op <="0001";
            end if;
            --half full and byte
            M2R <= "10";
        elsif curr_state = brn then
            pw <= p;
            Asrc1 <= "10";
            Asrc2 <= "11";
            aluW <='1';
            if B = '1' then
                RW <= '1';
                Wsrc <= "10";
                M2R <= "01";
            end if;
        end if;
    end process;      
end main_controller;

