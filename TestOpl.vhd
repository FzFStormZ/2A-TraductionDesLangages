--------------------------------------------------------------------------------
-- Company: 
-- Engineer:
--
-- Create Date:   09:26:39 11/10/2022
-- Design Name:   
-- Module Name:   /home/abecker/Documents/ENSEEIHT/N7/2A-ASR/S7/Archi/MiniProjet Joystick/fournitures/OpL/TestOpl.vhd
-- Project Name:  OpL
-- Target Device:  
-- Tool versions:  
-- Description:   
-- 
-- VHDL Test Bench Created by ISE for module: MasterOpl
-- 
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
-- Notes: 
-- This testbench has been automatically generated using types std_logic and
-- std_logic_vector for the ports of the unit under test.  Xilinx recommends
-- that these types always be used for the top-level I/O of a design in order
-- to guarantee that the testbench will bind correctly to the post-implementation 
-- simulation model.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY TestOpl IS
END TestOpl;
 
ARCHITECTURE behavior OF TestOpl IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT MasterOpl
    PORT(
         rst : IN  std_logic;
         clk : IN  std_logic;
         en : IN  std_logic;
         v1 : IN  std_logic_vector(7 downto 0);
         v2 : IN  std_logic_vector(7 downto 0);
         miso : IN  std_logic;
         ss : OUT  std_logic;
         sclk : OUT  std_logic;
         mosi : OUT  std_logic;
         val_and : OUT  std_logic_vector(7 downto 0);
         val_xor : OUT  std_logic_vector(7 downto 0);
         val_nor : OUT  std_logic_vector(7 downto 0);
         busy : OUT  std_logic
        );
    END COMPONENT;
   
    COMPONENT SlaveOpl
    PORT(
       sclk : IN std_logic;
       mosi : IN std_logic;
       ss : IN std_logic;          
       miso : OUT std_logic
       );
    END COMPONENT;

   --Inputs
   signal rst : std_logic := '0';
   signal clk : std_logic := '0';
   signal en : std_logic := '0';
   signal v1 : std_logic_vector(7 downto 0) := (others => '0');
   signal v2 : std_logic_vector(7 downto 0) := (others => '0');
   signal miso : std_logic := '0';

 	--Outputs
   signal ss : std_logic;
   signal sclk : std_logic;
   signal mosi : std_logic;
   signal val_and : std_logic_vector(7 downto 0);
   signal val_xor : std_logic_vector(7 downto 0);
   signal val_nor : std_logic_vector(7 downto 0);
   signal busy : std_logic;

   -- Clock period definitions
   constant clk_period : time := 10 ns;
 
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: MasterOpl PORT MAP (
          rst => rst,
          clk => clk,
          en => en,
          v1 => v1,
          v2 => v2,
          miso => miso,
          ss => ss,
          sclk => sclk,
          mosi => mosi,
          val_and => val_and,
          val_xor => val_xor,
          val_nor => val_nor,
          busy => busy
        );
      
   uut2: SlaveOpl PORT MAP (
          sclk => sclk,
          mosi => mosi,
          ss => ss,
          miso => miso
        );

   -- Clock process definitions
   clk_process :process
   begin
		clk <= '0';
		wait for clk_period/2;
		clk <= '1';
		wait for clk_period/2;
   end process;
 

   -- Stimulus process
   stim_proc: process
   begin		
      -- hold reset state for 100 ns.
      wait for 100 ns;
      rst <= '1';

      wait for clk_period*10;

      -- insert stimulus here
		-- signaux à simuler : rst, en, v1, v2
      -- signaux à observer : ss, sclk, mosi, val_and, val_xor, val_nor, busy, miso (input slave)
      
      v1 <= "10110101";
      v2 <= "01010101";
      en <= '1';

      wait for clk_period;
      en <= '0';
      v1 <= (others => '0');
      v2 <= (others => '0');

      wait for clk_period*80;
      v1 <= "10110101";
      v2 <= "11011101";
      en <= '1';

      wait for clk_period;
      en <= '0';
      v1 <= (others => '0');
      v2 <= (others => '0');

      wait for clk_period*80;
      v1 <= "10110101";
      v2 <= "01010101";
      en <= '1';

      wait for clk_period;
      en <= '0';
      v1 <= (others => '0');
      v2 <= (others => '0');

      wait;
   end process;

END;
