---------------------------------------------------------------------------------------------------------------
--Datapath+Controler
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity data_control is
port(
    clock : IN std_logic;
    reset : IN std_logic
    );
end data_control;

architecture data_control of data_control is
component datapath is 
PORT ( 
  clock        : in std_logic;
  reset        : in std_logic;
  ins          : out std_logic_vector(31 downto 0);
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
  shift_type : IN std_logic_vector(1 downto 0);  
  shift_enable : IN std_logic    
);
end component;

component main_controller is
port(
        clock        : IN std_logic;
 --       F : In std_logic_vector(1 downto 0);  
        reset        : in std_logic;
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
        BorS           : out  std_logic_vector(1 downto 0); --selects directly B or shifted B
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
end component;

  signal ins          :   std_logic_vector(31 downto 0);
  signal F            :   std_logic_vector(3 downto 0);
  signal PW           :   std_logic;
  signal IorD         :   std_logic;
  signal MR           :   std_logic;
  signal MW           :   std_logic;
  signal IW           :   std_logic;
  signal DW           :   std_logic;
  signal Rsrc1        :   std_logic; 
  signal Rsrc2        :   std_logic; 
  signal Shi          :   std_logic; 
  signal Shift        :   std_logic; 
  signal Shift_amount :  std_logic_vector(4 downto 0);
  signal Wsrc         :   std_logic_vector(1 downto 0);
  signal M2R          :   std_logic_vector(1 downto 0);  
  signal RW           :   std_logic;
  signal AW           :   std_logic;
  signal XW           :   std_logic;
  signal BW           :   std_logic;
  signal YW           :   std_logic;
  signal aluW         :   std_logic;
  signal mulW         :   std_logic;
  signal shftW        :   std_logic;
  signal BorS         :   std_logic_vector(1 downto 0); 
  signal Asrc1        :   std_logic_vector(1 downto 0);
  signal Asrc2        :   std_logic_vector(1 downto 0);
  signal MorA         :   std_logic;
  signal Fset         :   std_logic;
  signal alu_op       :   std_logic_vector(3 downto 0);
  signal p_m_path_op  :   std_logic_vector(3 downto 0);
  signal byte_offset  :   std_logic_vector(1 downto 0);
  signal ReW          :   std_logic;
  signal shift_type   :   std_logic_vector(1 downto 0);  
  signal shift_enable :   std_logic;  

begin

datapath_unit : datapath port map
( 
    clock        => clock,
    reset        => reset,
    ins          => ins,
    F            => F,
    PW           => PW,
    IorD         => IorD,
    MR           => MR,
    MW           => MW,
    IW           => IW,
    DW           => DW,
    Rsrc1        => Rsrc1, 
    Rsrc2        => Rsrc2,
    Shi          => shi,
    Shift        => shift ,
    Shift_amount => shift_amount,
    Wsrc         => Wsrc,
    M2R          => M2R,
    RW           => RW,
    AW           => AW,
    XW           => XW,
    BW           => BW,
    YW           => YW,
    aluW         => aluW,
    mulW         => mulW,
    shftW        => shftW,
    BorS         => BorS,
    Asrc1        => Asrc1,
    Asrc2        => Asrc2,
    MorA         => MorA,
    Fset         => Fset,
    alu_op       => alu_op,
    p_m_path_op  => p_m_path_op,
    byte_offset  => byte_offset,
    ReW          => ReW ,
    shift_type   => shift_type,
    shift_enable => shift_enable
);

controller: main_controller port map
( 
    clock        => clock,
    reset        => reset,
    ins          => ins,
    F            => F,
    PW           => PW,
    IorD         => IorD,
    MR           => MR,
    MW           => MW,
    IW           => IW,
    DW           => DW,
    Rsrc1        => Rsrc1, 
    Rsrc2        => Rsrc2,
    Shi          => shi,
    Shift        => shift ,
    Shift_amount => shift_amount,
    Wsrc         => Wsrc,
    M2R          => M2R,
    RW           => RW,
    AW           => AW,
    XW           => XW,
    BW           => BW,
    YW           => YW,
    aluW         => aluW,
    mulW         => mulW,
    shftW        => shftW,
    BorS         => BorS,
    Asrc1        => Asrc1,
    Asrc2        => Asrc2,
    MorA         => MorA,
    Fset         => Fset,
    alu_op       => alu_op,
    p_m_path_op  => p_m_path_op,
    byte_offset  => byte_offset,
    ReW          => ReW ,
    shift_type   => shift_type,
    shift_enable => shift_enable
);


end data_control;