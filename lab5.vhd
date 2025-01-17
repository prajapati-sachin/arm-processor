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
--p <= '1';
    
end Bctrl;

---------------------------------------------------------------------------------------------------------------
-- REGISTER
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity register_0 is
port(
    wr : IN  std_logic;
    enable: IN std_logic; 
    --clock : IN std_logic;  
  rd : OUT std_logic  
  );
end register_0;

architecture register_0 of register_0 is
signal data : std_logic;
begin
    rd <= data;
    process(enable,wr)
        begin
        --if clock'event and clock = '1' then
        if enable ='1' then
                data <= wr;
        end if;
        --end if;
    end process;

end register_0;


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
signal d : std_logic_vector(2 downto 0);
begin
    d(1 downto 0) <= F;
    d(2) <= DT_U;
    with d select
        alu_op <= DP_op when "000",
                  DP_op when "100",
                  "0100"when "101",
                  "0010"when "001",
                  "0100"when others;
--    process(DP_op,DT_U,F)
--    begin
--        if F = "00" then
--            alu_op <= DP_op;
--        elsif F = "01" then 
--            if DT_U = '1' then
--                alu_op <= "0100";
--            else
--                alu_op <= "0010";
--            end if;
--        elsif F="10" then 
--            alu_op <= "0100";
--        end if;
--    end process;
    
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
    ShTyp : OUT std_logic_vector(1 downto 0);
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
signal temp_amount : std_logic_vector(4 downto 0):= "00000";
begin
   process(Instruction)
        begin
        if Instruction(27 downto 26) = "00" then
              F <= "00"; 
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
                               temp_amount(3 downto 0) <= Instruction(11 downto 8) ;  
                               sh_amount <= temp_amount + temp_amount;  
                               
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
                                                alu_op <= Instruction(24 downto 21);
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
                          
                     end if;
            if Instruction(27 downto 26) = "01" then
              F <= "01";
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
                          
            end if;
            if Instruction(27 downto 26) = "10" then
             F <="10";
                          B <= Instruction(24);
                          --alu_op <= "0100";
                          
            end if;
            if Instruction(27 downto 26) = "11" then
             INVALID <= '1';
             end if;
                
        
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
        clock        : IN std_logic;
        push_button        : IN std_logic;
 --       F : In std_logic_vector(1 downto 0);  
        reset        : in std_logic;
        pr : in std_logic; 
        ins          : in std_logic_vector(31 downto 0);
        hready       : in std_logic;
        hsize        : out  std_logic_vector(2 downto 0);
        M           : out  std_logic;
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
        Shift_amount : out std_logic_vector(4 downto 0);
        Wsrc         : out  std_logic_vector(1 downto 0);
        M2R          : out  std_logic_vector(1 downto 0); -- selects REW , Bout or 
        RW           : out  std_logic;
        AW           : out  std_logic;
        XW           : out  std_logic;
        BW           : out  std_logic;
        YW           : out  std_logic;
        aluW           : out  std_logic;
        mulW           : out  std_logic;
        shftW           : out  std_logic;
        BorS           : out  std_logic_vector(1 downto 0); --selects directly B or shifted B or mla
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
    ShTyp : OUT std_logic_vector(1 downto 0);
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
    DT_U : OUT std_logic ;
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

component register_0 is
port(
    wr : IN  std_logic;
    enable: IN std_logic; 
    --clock : IN std_logic;  
  rd : OUT std_logic  
  );
end component;

type State IS (     
                   start,
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
signal curr_state :State := start;
signal  alu_opd :  std_logic_vector(3 downto 0);  
signal  Fol :  std_logic_vector(1 downto 0) := "00";  
signal  Fo :  std_logic_vector(1 downto 0);  
signal  DP_imm:  std_logic;
signal  no_result:  std_logic;
signal  INVALID:  std_logic;
signal  immediate:  std_logic_vector(7 downto 0);
signal  alu_operation:  std_logic_vector(3 downto 0);
signal  ShTyp :  std_logic_vector(1 downto 0);
signal  Sh_amount :  std_logic_vector(4 downto 0);
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
signal  m1 :  std_logic;
signal  S :  std_logic;
signal  p:  std_logic; --predicate
signal  predicate:  std_logic:= '0'; --predicate
signal  predicateW:  std_logic; --predicate


    
begin
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
    DT_immediate  => DT_immediate,
    B => B,
    S => S  
);

Acontrol : Actrl port map
(
    F => Fo,
    DP_op => alu_operation, 
    DT_U => DT_U,
    alu_op => alu_opd
);

Bcontrol : Bctrl port map
(
    flags => F,  
    instr => ins(31 downto 28),
    p => predicate
);

p_reg : register_0 port map
(   wr => predicate,
    enable => predicateW,
   -- clock => clock,
    rd => p
);



    process(clock,push_button)
    begin
        if clock'event  and clock ='1' then
            if curr_state = start then
                if push_button = '1' then
                    curr_state <= fetch;
                end if;
            end if;
            if curr_state = fetch then
                if pr = '1' then
                    curr_state <= start;
                end if;
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
                if mla_e = '1'then
                    curr_state <= mul;
                else
                    curr_state <= shft;
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
                if hready = '1' then
                    curr_state <= fetch;
                end if;
            end if;
            if curr_state = rdM then
                if hready = '1' then
                    curr_state <= M2RF;
                end if;
            end if;
            if curr_state = M2RF then
                curr_state <= fetch;
            end if;
            if curr_state = brn then
                curr_state <= fetch;
            end if;
        end if;
    end process;
    

    
    process(clock)
    begin
    if clock'event and clock ='1' then

    PW           <= '0';
   -- IorD         <= '0';
    MR           <= '0';
    MW           <= '0';
    IW           <= '0';
    DW           <= '0';
  --  Rsrc1        <= '0'; --control signal for operand B
  --  Rsrc2        <= '0';--control signal for operand A
  --  Shi          <= '0'; --shift from register or immediate
  --  Shift        <= '0'; --shiftamount( from register '1' and from intruction '0')
    Shift_amount <= "00000";
--    Wsrc         <="00";
   -- M2R          <="01"; -- selects REW , Bout or 
    RW           <= '0';
    AW           <= '0';
    XW           <= '0';
    BW           <= '0';
    aluW           <= '0';
    mulW           <= '0';
    shftW           <= '0';
  --  BorS           <= '0'; --selects directly B or shifted B
  --  Asrc1        <="00";
  --  Asrc2        <="00";
  --  MorA        <= '0';
    Fset         <= '0';
    p_m_path_op  <= "0000";
    byte_offset  <="00";
    ReW          <= '0';
    M1           <= '0';
    shift_type <="00" ;
    shift_enable <= '1';
    predicateW <= '0';
        if curr_state = fetch then
            IorD <= '0';
            PW <= '1';
            IW <= '1';
            DW <= '0';
            MR <= '1';
            Asrc1 <= "10";
            Asrc2 <= "01";
            alu_op <= "0100";
--            aluW <= '1';
--            MorA <= '1';
        elsif curr_state = rdAB then
            predicateW <= '1';
            AW <= '1';
            BW <= '1';
            if Fo = "00" then
                if alu_e = '1' then
                    Rsrc2 <= '0';
                    Rsrc1 <= '0';
                else
                    Rsrc2 <= '1';
                    Rsrc1 <= '0';
                end if;
            else
                Rsrc2 <= '0';
                Rsrc1 <= '0';
            end if;
        elsif curr_state = rdrs then
            XW <= '1';
            Rsrc2 <= '1';
            if mla_e = '1' then
                YW <= '1';
                Rsrc1 <= '1';
            end if;
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
            Wsrc <= "01";
            M2R <= "01";
            if mla_e = '0' then
                MorA <= '0';
                ReW <= '1';
            else 
                REW <= '0';
            end if;
        elsif curr_state = arith then
            if alu_e = '1' then
                Asrc1 <= "00";
                BorS <= "01";
                Asrc2 <= "00"; 
                Wsrc <= "00";
                M2R <= "01";  
            elsif mla_e = '1' then
                Asrc1 <= "01";
                BorS <= "10";
                Asrc2 <= "00";
            end if;
            if S = '1' then
                Fset <= p;
            end if;
            alu_op <= alu_operation;
            aluW <='1';
            MorA <= '1';
            ReW <= '1';
        elsif curr_state = wrRF then
            if alu_e = '1' then
               
                Wsrc <= "00";
                M2R <= "01";
                if no_result = '1' then
                    RW <= '0';
                else
                    RW <= p;
                end if;
            else 
                RW <= p;
                Wsrc <= "01";
                M2R <= "01";
            end if;
                
        elsif curr_state = addr then
            Asrc1 <= "00";
            if DT_reg = '1' then
                BorS <= "01";
                Asrc2 <= "00";
            else
                BorS <= "01";
                Asrc2 <= "10";
            end if;
            if DT_U = '1' then
                alu_op <= "0100";
            else 
                alu_op <= "0101";
            end if;
            aluW <='1';
            BW <= '1';
            Rsrc1 <= '1';
            MorA <= '1';
            ReW <= '1';
        elsif curr_state = wrM then
            MW <= p;
            IorD <= '1';
            if m1 = '0' then
                m1 <= p;
            else 
                m1 <= '0';
            end if;
                
            if DT_half = '1' then
                p_m_path_op <= "0010";
                byte_offset <= "01";
                hsize <= "001";
            elsif DT_Byte = '1' then
                p_m_path_op <= "0101";
                byte_offset <= "11";
                hsize <= "000";
            else
                p_m_path_op <="0001";
                hsize <= "010";
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
            M1 <= '1';
            if hready = '1' then
                Wsrc <= "00";
                M2r <= "10";
            end if;
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
            Wsrc <= "00";
            M2R <= "10";
        elsif curr_state = brn then
            pw <= p;
            Asrc1 <= "10";
            Asrc2 <= "11";
            if B = '1' then
                RW <= '1';
                Wsrc <= "10";
                M2R <= "01";
            end if;
        end if;
        end if;
    end process;  
    
    m <= m1;    
end main_controller;

