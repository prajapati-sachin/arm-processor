---------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------
-- MUX_4_0
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity mux_4_0 is
port(
    data1: IN  std_logic;
    data2: IN  std_logic;
    data3: IN  std_logic;
    data4: IN  std_logic; 
    cntrl : IN std_logic_vector(1 downto 0);
  data : OUT std_logic 
  );
end mux_4_0;

architecture mux of mux_4_0 is
--signal data2: std_logic_vector(31 downto 0):="00000000000000000000000000000100";
begin
    with cntrl select
        data <= data1 when "00",
                data2 when "01",
                data3 when "10",
                data4 when others;
end mux;

---------------------------------------------------------------------------------------------------------------
--MUX_2_31bit
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity mux_31 is
port(
    data1: IN  std_logic_vector(31 downto 0);
    data2: IN  std_logic_vector(31 downto 0); 
    cntrl : IN std_logic;
  data : OUT std_logic_vector(31 downto 0)  
  );
end mux_31;

architecture mux of mux_31 is
begin
    with cntrl select
        data <= data1 when '0',
                data2 when others;
end mux;

-----------------------------------------------------------------------------------------------------
--CLOCK
-----------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity clocking is
    port(
        clk : IN  std_logic;
        slow_clock : IN  std_logic;
        a_clock : OUT std_logic
        );
end clocking;

architecture l5_clock of clocking is
    signal slowclock : std_logic_vector (16 downto 0):="00000000000000000";
    begin
        process(clk, slow_clock)
        begin
            if slow_clock = '0' then
                if clk'event and clk='1' then
                    slowclock <= slowclock + 1;
                end if;
                a_clock <= slowclock(16);
            else 
                a_clock <= clk;
            end if;
        end process;
end l5_clock;
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--ANODE
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
 use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;
 
 
entity anode_1 is
         Port ( a_clock : in  STD_LOGIC;
                anode : out  STD_LOGIC_VECTOR (3 downto 0));
end anode_1;
 
architecture anode1 of anode_1 is
     signal a_tmp: std_logic_vector(3 downto 0):= "0111";
     begin
     process(a_clock)
         begin
                if a_clock'event and a_clock='1' then
                  a_tmp(1) <= a_tmp(0);
                  a_tmp(2) <= a_tmp(1);
                  a_tmp(3) <= a_tmp(2);
                  a_tmp(0) <= a_tmp(3);
                end if;
     end process;
     anode <= a_tmp;
end anode1;

---------------------------------------------------------------------------------------------------------------
--DISPLAY
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity display is
    port( anode : in std_logic_vector(3 downto 0);
          input : in std_logic_vector(15 downto 0);
          cathode : out std_logic_vector(6 downto 0)
        );

end display;

architecture dis of display is
begin
    process(anode,input)
    begin
    if anode="0111" then
        case input(15 downto 12) is 
                when "0000"=> cathode <="1000000";  
                when "0001"=> cathode <="1111001";
                when "0010"=> cathode <="0100100";
                when "0011"=> cathode <="0110000";
                when "0100"=> cathode <="0011001"; 
                when "0101"=> cathode <="0010010";
                when "0110"=> cathode <="0000010";
                when "0111"=> cathode <="1111000";
                when "1000"=> cathode <="0000000";
                when others=> cathode <="0010000";
             
        end case;
    end if;

    if anode="1011" then
        case input(11 downto 8) is 
                when "0000"=> cathode <="1000000";  
                when "0001"=> cathode <="1111001";
                when "0010"=> cathode <="0100100";
                when "0011"=> cathode <="0110000";
                when "0100"=> cathode <="0011001"; 
                when "0101"=> cathode <="0010010";
                when "0110"=> cathode <="0000010";
                when "0111"=> cathode <="1111000";
                when "1000"=> cathode <="0000000";
                when others=> cathode <="0010000";
       end case;
    end if;

    if anode="1101" then
        case input(7 downto 4) is 
                when "0000"=> cathode <="1000000";  
                when "0001"=> cathode <="1111001";
                when "0010"=> cathode <="0100100";
                when "0011"=> cathode <="0110000";
                when "0100"=> cathode <="0011001"; 
                when "0101"=> cathode <="0010010";
                when "0110"=> cathode <="0000010";
                when "0111"=> cathode <="1111000";
                when "1000"=> cathode <="0000000";
                when others=> cathode <="0010000";
       end case;
    end if;

    if anode="1110" then 
        case input(3 downto 0) is 
                when "0000"=> cathode <="1000000";  
                when "0001"=> cathode <="1111001";
                when "0010"=> cathode <="0100100";
                when "0011"=> cathode <="0110000";
                when "0100"=> cathode <="0011001"; 
                when "0101"=> cathode <="0010010";
                when "0110"=> cathode <="0000010";
                when "0111"=> cathode <="1111000";
                when "1000"=> cathode <="0000000";
                when others=> cathode <="0010000";
       end case;
    end if;    
end process;
end dis;
-- 😎 😎 😎 😎 😎 😎 😎
---------------------------------------------------------------------------------------------------------------
--MASTER INTERFACE
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity master_interface is
port(
    HREADY : IN STD_LOGIC;
    push_button : IN std_logic;
    HRESETn : IN STD_LOGIC;
    HCLK  : IN STD_LOGIC;
--    HWRITE_ENABLE : IN STD_LOGIC;
    HRDATA : IN STD_LOGIC_VECTOR (31 downto 0);
    HWDATA : OUT STD_LOGIC_VECTOR (31 downto 0);
    HADDR : OUT STD_LOGIC_VECTOR (15 downto 0);
    HSIZE : OUT STD_LOGIC_VECTOR (2 downto 0);
    HTRANS : OUT STD_LOGIC_VECTOR (1 downto 0);
    reg          : out std_logic_vector(31 downto 0);
    HWRITE : OUT STD_LOGIC
    );
end master_interface;

architecture master_interface of master_interface is

component data_control
port(
    clk : IN std_logic;
    push_button : IN std_logic;
    hready       : in std_logic;
    hsize        : out  std_logic_vector(2 downto 0);
    HWDATA       : out std_logic_vector(31 downto 0);
    M           : out  std_logic;
    MemR          :out    std_logic;
    MemW           :out    std_logic;
    HADDR       : out std_logic_vector(15 downto 0);
    HRDATA       : in std_logic_vector(31 downto 0);
    reg          : out std_logic_vector(31 downto 0);
    reset : IN std_logic
);
end component;
type State IS (     
                neutral,
                trans,
                idle
                --data_transfer                      
);
signal curr_state :State := neutral;
signal M            :std_logic;
signal ready        :std_logic;
signal wdata        :std_logic_vector(31 downto 0);
signal size         :std_logic_vector(2 downto 0);
signal addr         :std_logic_vector(15 downto 0);
signal rdata        :std_logic_vector(31 downto 0);
signal MemR         :std_logic;
signal MemW         :std_logic;


begin

processor : data_control port map
(
    clk         => HCLK,
    push_button => push_button,
    hready      => HREADY,
    hsize       => SIZE,
    HWDATA      => WDATA,
    M           => M,
    MemR        => MemR,
    MemW        => MemW,
    HADDR       => ADDR,
    HRDATA      => RDATA,
    reg         => reg,
    reset       => HRESETn
);

process(HCLK,HRESEtn,HREADY)
begin
    if HCLK'event and HCLK = '1' then
        if curr_state = neutral then
            if M = '1' then
                curr_state <= trans;
            end if;
        end if;
        if curr_state = trans then
            curr_state <= idle;
        end if;
        if curr_state = idle then
            if HREADY = '1' then
                curr_state <= neutral;
--                if MemW = '1' then
--                    curr_state <= data_transfer;
--                else 
--                    curr_state <= neutral;
--                end if;
            end if;
        end if;
--        if curr_state = data_transfer then
--            curr_state <= neutral;
--        end if;
        
        if HRESETn = '1' then
            curr_state <= neutral;
        end if;
    end if;
end process;

process(HCLK,curr_state)
begin
    if HCLK'event and HCLK = '1' then
        HTRANS <= "00";
        if curr_state = trans then
            HTRANS <= "01";
            HADDR <= addr;
            HSIZE <= size;
            if MemW = '1' then
                HWRITE <= '1';
                HWDATA <= wdata;
            else 
                HWRITE <= '0';
            end if;
        end if;
        if curr_state = idle then
            if HREADY = '1' then
                rdata <= HRDATA;
                ready <= '1';
            end if;
        end if;
        

    end if;
end process;
end master_interface;


-- 😎 😎 😎 😎 😎 😎 😎
---------------------------------------------------------------------------------------------------------------
--SLAVE INTERFACE MEMORY
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity slave_interface_memory is
port(
    HREADY : OUT STD_LOGIC;
    HSELx : IN STD_LOGIC;
    HRESETn : IN STD_LOGIC;
    HCLK  : IN STD_LOGIC;
    push_button  : IN STD_LOGIC;
--    HWRITE_ENABLE : OUT STD_LOGIC;
    HRDATA : OUT STD_LOGIC_VECTOR (31 downto 0);
    HWDATA : IN STD_LOGIC_VECTOR (31 downto 0);
    HADDR : IN STD_LOGIC_VECTOR (5 downto 0);
    HSIZE : IN STD_LOGIC_VECTOR (2 downto 0);
    HTRANS : IN STD_LOGIC_VECTOR (1 downto 0);
    HWRITE : IN STD_LOGIC
    );
end slave_interface_memory;

architecture slave_interface_memory of slave_interface_memory is
component data_memory is
port(
    wr_data : IN  std_logic_vector(31 downto 0);
    addr : IN std_logic_vector(5 downto 0); 
    write_enable : IN std_logic;
    push_button : IN std_logic;    
    clock : IN std_logic;    
    rd_data : OUT std_logic_vector(31 downto 0) 
        
    );
end component;

type State IS (     
                neutral,
                wait_state1,
                wait_state2,
                wait_state3                    
);
signal curr_state :State := neutral;
signal wdata        :std_logic_vector(31 downto 0);
signal size         :std_logic_vector(2 downto 0);
signal addr         :std_logic_vector(5 downto 0);
signal rdata        :std_logic_vector(31 downto 0);
signal we           :std_logic;
signal w            :std_logic;


begin

memory_block : data_memory port map
(
    wr_data     => HWDATA, 
    addr        => addr ,
    write_enable=> we,
    push_button => push_button,
    clock       => HCLK,    
    rd_data     => rdata        
);

process(HCLK,HRESEtn)
begin
    if HCLK'event and HCLK = '1' then
        if curr_state = neutral then
            if HTRANS = "01" then
                if HSELx = '1' then
                    curr_state <= wait_state1;
                end if;
            end if;
        end if;
        if curr_state = wait_state1 then
            curr_state <= wait_state2;
        end if;
        if curr_state = wait_state2 then
            curr_state <= wait_state3;
        end if;
        if curr_state = wait_state3 then
            curr_state <= neutral;
        end if;        
        if HRESETn = '1' then
            curr_state <= neutral;
        end if;
    end if;
end process;


process(HCLK,curr_state)
begin
    if HCLK'event and HCLK = '1' then
        if curr_state = neutral then
            if HTRANS = "01" then
                addr <= HADDR;
                W <= HWRITE; 
                HREADY <= '0';               
            end if;
        end if;
        if curr_state = wait_state1 then
            HREADY <= '0';
        end if;
        if curr_state = wait_state2 then
            HREADY <= '0';
        end if;
        if curr_state = wait_state3 then
            HREADY <= '1';
            if w = '1' then
                we <= '1';
            else 
                HRDATA <= rdata;
            end if;
        end if;        
    end if;
end process;

    
end slave_interface_memory;

---------------------------------------------------------------------------------------------------------------
-- MEMORY
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity data_memory is
port(
--    write_enables : in std_logic_vector(3 downto 0);
    wr_data : IN  std_logic_vector(31 downto 0);
    addr : IN std_logic_vector(5 downto 0); 
    write_enable : IN std_logic;
--    read_enable : IN std_logic;
    push_button : IN std_logic;    
    clock : IN std_logic;    
    rd_data : OUT std_logic_vector(31 downto 0) 
        
    );
end data_memory;
--00 for word 01 for hLF WORD 10 FOR BYTE
architecture data_memory of data_memory is
type memory_array is array(0 to 63) of std_logic_vector(31 downto 0);
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
    rd_data <= data_memory(address);
    process(wr_data,clock,address,write_enable,push_button)
        begin
        if push_button = '1' then
            data_memory(5) <= "00000000000000000000000000001001";
        end if;  
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

-- 😎 😎 😎 😎 😎 😎 😎
---------------------------------------------------------------------------------------------------------------
--SLAVE INTERFACE Switches
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity slave_interface_switches is
port(
    HREADY : OUT STD_LOGIC;
    HSELx : IN STD_LOGIC;
    HRESETn : IN STD_LOGIC;
    push_button  : IN STD_LOGIC;
    HCLK  : IN STD_LOGIC;
    HRDATA : OUT STD_LOGIC_VECTOR (31 downto 0);
    HTRANS : IN STD_LOGIC_VECTOR (1 downto 0);
    Switches : IN STD_LOGIC_VECTOR (15 downto 0);
    HWRITE : IN STD_LOGIC
    );
end slave_interface_switches;

architecture slave_interface_switches of slave_interface_switches is


type State IS (     
                neutral,
                data_transfer                  
);
signal curr_state :State := neutral;
signal rdata        :std_logic_vector(31 downto 0):= (others => '0');

begin
process(HCLK,HRESEtn)
begin
    if HCLK'event and HCLK = '1' then
        if curr_state = neutral then
            if HTRANS = "01" then
                if HSELx = '1' then
                    if HWRITE = '0' then
                        curr_state <= data_transfer;
                    end if;
                end if;
            end if;
            HREADY <= '0';
        end if;
        if curr_state = data_transfer then
            curr_state <= neutral;
            HREADY <= '1';
        end if;
        if HRESETn = '1' then
            curr_state <= neutral;
        end if;
    end if;
end process;


process(HCLK,curr_state,push_button)
begin
    if HCLK'event and HCLK = '1' then
        if push_button = '1' then
            rdata(15 downto 0) <= Switches;
        end if;
        if curr_state = data_transfer then
            HRDATA <= rdata;
        end if;
    end if;
    
end process;


    
end slave_interface_switches;

-- 😎 😎 😎 😎 😎 😎 😎
---------------------------------------------------------------------------------------------------------------
--SLAVE INTERFACE LEDs
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity slave_interface_LEDs is
port(
    HREADY : OUT STD_LOGIC;
    HSELx : IN STD_LOGIC;
    HRESETn : IN STD_LOGIC;
    HCLK  : IN STD_LOGIC;
    HWDATA : IN STD_LOGIC_VECTOR (31 downto 0);
    HTRANS : IN STD_LOGIC_VECTOR (1 downto 0);
    LEDs    : OUT STD_LOGIC_VECTOR (15 downto 0);
    HWRITE : IN STD_LOGIC
    );
end slave_interface_LEDs;

architecture slave_interface_LEDs of slave_interface_LEDs is


type State IS (     
                neutral,
                data_transfer                  
);
signal curr_state :State := neutral;
signal rdata        :std_logic_vector(31 downto 0) := (others => '0');

begin
LEDs <= rdata(15 downto 0);
process(HCLK,HRESEtn)
begin
    if HCLK'event and HCLK = '1' then
        if curr_state = neutral then
            if HTRANS = "01" then
                if HSELx = '1' then
                    if HWRITE = '1' then
                        curr_state <= data_transfer;
                    end if;
                end if;
            end if;
                        HREADY <= '0';

        end if;
        if curr_state = data_transfer then
            curr_state <= neutral;
            HREADY <= '1';
        end if;
      
        if HRESETn = '1' then
            curr_state <= neutral;
        end if;
    end if;
end process;


process(HCLK,curr_state)
begin
    if HCLK'event and HCLK = '1' then
        if curr_state = data_transfer then
            rdata <= HWDATA;
        end if;
    end if;
end process;

    
end slave_interface_LEDs;


-- 😎 😎 😎 😎 😎 😎 😎
---------------------------------------------------------------------------------------------------------------
--SLAVE INTERFACE 7segment display
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity slave_interface_display is
port(
    HREADY : OUT STD_LOGIC;
    HSELx : IN STD_LOGIC;
    HRESETn : IN STD_LOGIC;
    HCLK  : IN STD_LOGIC;
    HWDATA : IN STD_LOGIC_VECTOR (31 downto 0);
    HTRANS : IN STD_LOGIC_VECTOR (1 downto 0);
    BCD : OUT STD_LOGIC_VECTOR (15 downto 0);
    HWRITE : IN STD_LOGIC
    );
end slave_interface_display;

architecture slave_interface_display of slave_interface_display is


type State IS (     
                neutral,
                data_transfer                  
);
signal curr_state :State := neutral;
signal rdata        :std_logic_vector(31 downto 0):= (others => '0' ) ;

begin

BCD <= rdata(15 downto 0);
process(HCLK,HRESEtn)
begin
    if HCLK'event and HCLK = '1' then
        if curr_state = neutral then
            HREADY <= '0';
            if HTRANS = "01" then
                if HSELx = '1' then
                    if HWRITE = '1' then
                        curr_state <= data_transfer;
                        HREADY <= '1';
                    end if;
                end if;
            end if;
            
        end if;
        if curr_state = data_transfer then
            curr_state <= neutral;
            HREADY <= '1';
        end if;
      
        if HRESETn = '1' then
            curr_state <= neutral;
        end if;
    end if;
end process;


process(HCLK,curr_state)
begin
    if HCLK'event and HCLK = '1' then
        if curr_state = data_transfer then
            rdata <= HWDATA;
        end if;
    end if;
end process;

    
end slave_interface_display;




-- 😎 😎 😎 😎 😎 😎 😎
---------------------------------------------------------------------------------------------------------------
--BUS
-----------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
USE ieee.std_logic_unsigned.all;
entity ahb_bus is
port(
    clk :in std_logic;
    slow_clock : IN  std_logic;
    reset :in std_logic;
    LEDs   :out std_logic_vector(15 downto 0);
    Switches   :in std_logic_vector(15 downto 0);
    Cathode :out std_logic_vector(6 downto 0);
    Anode   :out std_logic_vector(3 downto 0);
    push_button : in std_logic
    );
end ahb_bus;

architecture ahb_bus of ahb_bus is

component master_interface is
port(
    HREADY : IN STD_LOGIC;
    push_button : IN std_logic;
    HRESETn : IN STD_LOGIC;
    HCLK  : IN STD_LOGIC;
--    HWRITE_ENABLE : IN STD_LOGIC;
    HRDATA : IN STD_LOGIC_VECTOR (31 downto 0);
    HWDATA : OUT STD_LOGIC_VECTOR (31 downto 0);
    HADDR : OUT STD_LOGIC_VECTOR (15 downto 0);
    HSIZE : OUT STD_LOGIC_VECTOR (2 downto 0);
    HTRANS : OUT STD_LOGIC_VECTOR (1 downto 0);
    reg          : out std_logic_vector(31 downto 0);
    HWRITE : OUT STD_LOGIC
    );
end component;

component slave_interface_memory is
port(
    HREADY : OUT STD_LOGIC;
    HSELx : IN STD_LOGIC;
    HRESETn : IN STD_LOGIC;
    HCLK  : IN STD_LOGIC;
    push_button  : IN STD_LOGIC;
--    HWRITE_ENABLE : OUT STD_LOGIC;
    HRDATA : OUT STD_LOGIC_VECTOR (31 downto 0);
    HWDATA : IN STD_LOGIC_VECTOR (31 downto 0);
    HADDR : IN STD_LOGIC_VECTOR (5 downto 0);
    HSIZE : IN STD_LOGIC_VECTOR (2 downto 0);
    HTRANS : IN STD_LOGIC_VECTOR (1 downto 0);
    HWRITE : IN STD_LOGIC
    );
end component;

component slave_interface_switches is
port(
    HREADY : OUT STD_LOGIC;
    HSELx : IN STD_LOGIC;
    HRESETn : IN STD_LOGIC;
    HCLK  : IN STD_LOGIC;
    push_button  : IN STD_LOGIC;
    HRDATA : OUT STD_LOGIC_VECTOR (31 downto 0);
    HTRANS : IN STD_LOGIC_VECTOR (1 downto 0);
    Switches : IN STD_LOGIC_VECTOR (15 downto 0);
    HWRITE : IN STD_LOGIC
    );
end component;

component slave_interface_LEDs is
port(
    HREADY : OUT STD_LOGIC;
    HSELx : IN STD_LOGIC;
    HRESETn : IN STD_LOGIC;
    HCLK  : IN STD_LOGIC;
    HWDATA : IN STD_LOGIC_VECTOR (31 downto 0);
    HTRANS : IN STD_LOGIC_VECTOR (1 downto 0);
    LEDs    : OUT STD_LOGIC_VECTOR (15 downto 0);
    HWRITE : IN STD_LOGIC
    );
end component;

component slave_interface_display is
port(
    HREADY : OUT STD_LOGIC;
    HSELx : IN STD_LOGIC;
    HRESETn : IN STD_LOGIC;
    HCLK  : IN STD_LOGIC;
    HWDATA : IN STD_LOGIC_VECTOR (31 downto 0);
    HTRANS : IN STD_LOGIC_VECTOR (1 downto 0);
    BCD : OUT STD_LOGIC_VECTOR (15 downto 0);
    HWRITE : IN STD_LOGIC
    );
end component;

component mux_4_0 is
port(
    data1: IN  std_logic;
    data2: IN  std_logic;
    data3: IN  std_logic;
    data4: IN  std_logic; 
    cntrl : IN std_logic_vector(1 downto 0);
  data : OUT std_logic 
  );
end component;

component mux_31 is
port(
    data1: IN  std_logic_vector(31 downto 0);
    data2: IN  std_logic_vector(31 downto 0); 
    cntrl : IN std_logic;
  data : OUT std_logic_vector(31 downto 0)  
  );
end component;

component clocking is
port(
        clk : IN  std_logic;
        slow_clock : IN  std_logic;
        a_clock : OUT std_logic
        );
end component;

component anode_1 is
         Port ( a_clock : in  STD_LOGIC;
                anode : out  STD_LOGIC_VECTOR (3 downto 0));
end component;

component display is
    port( anode : in std_logic_vector(3 downto 0);
          input : in std_logic_vector(15 downto 0);
          cathode : out std_logic_vector(6 downto 0)
        );

end component;

signal HREADY :  STD_LOGIC;
signal HREADYm :  STD_LOGIC;
signal HREADYs :  STD_LOGIC;
signal HREADYl :  STD_LOGIC;
signal HREADYd :  STD_LOGIC;
signal HSELm :  STD_LOGIC:= '0';
signal HSELl :  STD_LOGIC:= '0';
signal HSELd :  STD_LOGIC:= '0';
signal HSELs :  STD_LOGIC:= '0';
signal HRESETn :  STD_LOGIC;
signal HRDATA :  STD_LOGIC_VECTOR (31 downto 0);
signal HRDATAm :  STD_LOGIC_VECTOR (31 downto 0);
signal HRDATAs :  STD_LOGIC_VECTOR (31 downto 0);
signal HWDATA :  STD_LOGIC_VECTOR (31 downto 0);
signal BCD :  STD_LOGIC_VECTOR (15 downto 0);
signal HADDR :  STD_LOGIC_VECTOR (15 downto 0);
signal HADDR1 :  STD_LOGIC_VECTOR (5 downto 0);
signal HSIZE :  STD_LOGIC_VECTOR (2 downto 0);
signal HTRANS :  STD_LOGIC_VECTOR (1 downto 0);
signal HWRITE :  STD_LOGIC;
signal slow_clk :  STD_LOGIC;
signal anode1 :  STD_LOGIC_VECTOR (3 downto 0);
signal ledss :  STD_LOGIC_VECTOR (15 downto 0);
signal regl :  STD_LOGIC_VECTOR (31 downto 0);

begin

slow_cl : clocking port map
(
        clk => clk,
        slow_clock => slow_clock,
        a_clock => slow_clk
);

anod : anode_1 port map
( 
    a_clock => slow_clk,
    anode  => anode1
);
anode <= anode1;
dis : display port map
( 
    anode => anode1,
    input => BCD,
    cathode => cathode
);


HADDR1 <= HADDR(5 downto 0);
master : master_interface port map 
(
    HREADY => HREADY,
    push_button  => push_button,
    HRESETn => reset,
    HCLK    => clk,
--    HWRITE_ENABLE : OUT STD_LOGIC;
    HRDATA => HRDATA,
    HWDATA => HWDATA,
    HADDR  => HADDR,
    HSIZE  => HSIZE,
    HTRANS => HTRANS,
    reg   => regl,
    HWRITE => HWRITE
);

slave_memory : slave_interface_memory port map
(
    HREADY => HREADYm,
    HSELx  => HSELm,
    HRESETn => reset,
    HCLK    => clk,
    push_button  => push_button,
--    HWRITE_ENABLE : OUT STD_LOGIC;
    HRDATA => HRDATAm,
    HWDATA => HWDATA,
    HADDR  => HADDR1,
    HSIZE  => HSIZE,
    HTRANS => HTRANS,
    HWRITE => HWRITE
);

slave_switches : slave_interface_switches port map
(   
    HREADY => HREADYs,
    HSELx => HSELs,
    HRESETn => reset,
    HCLK  => clk,
    push_button  => push_button,
    HRDATA => HRDATAs,
    HTRANS => HTRANS,
    Switches => Switches,
    HWRITE => HWRITE
);

slave_LEDS : slave_interface_LEDs port map
(   HREADY => HREADYl,
    HSELx => HSELl,
    HRESETn => reset,
    HCLK  => clk,
    HWDATA => HWDATA,
    HTRANS => HTRANS,
    LEDS   => LEDs,
    HWRITE => HWRITE
);

slave_display : slave_interface_display port map
(       
    HREADY => HREADYd,
    HSELx => HSELd,
    HRESETn => reset,
    HCLK  => clk,
    HWDATA => HWDATA,
    HTRANS => HTRANS,
    BCD => BCD,
    HWRITE => HWRITE
);


mux_for_HREADY : mux_4_0 port map
(   data1 => HREADYm,
    data2 => HREADYs,
    data3 => HREADYl,
    data4 => HREADYd,
    cntrl => HADDR(7 downto 6),
    data => HREADY
); 

mux_for_HRDATA : mux_31 port map
(   data1 => HRDATAm,
    data2 => HRDATAs,
    cntrl => HADDR(6),
    data => HRDATA
); 

HSELm <= (not HADDR(6)) and (not HADDR(7)); 
HSELs <= (HADDR(6)) and (not HADDR(7)); 
HSELl <= (not HADDR(6)) and ( HADDR(7)); 
HSELd <= ( HADDR(6)) and ( HADDR(7)); 

--leds <= regl(15 downto 0);
--leds <= "0000111100001111";

end ahb_bus;


