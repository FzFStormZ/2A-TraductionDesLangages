library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity MasterOpl is
  port ( rst : in std_logic;
         clk : in std_logic;
         en : in std_logic;
         v1 : in std_logic_vector (7 downto 0);
         v2 : in std_logic_vector(7 downto 0);
         miso : in std_logic;
         ss   : out std_logic;
         sclk : out std_logic;
         mosi : out std_logic;
         val_and : out std_logic_vector (7 downto 0);
         val_xor : out std_logic_vector (7 downto 0);
         val_nor : out std_logic_vector (7 downto 0);
         busy : out std_logic);
end MasterOpl;

architecture behavior of MasterOpl is

	type t_etat is (idle, attente, echange);
	signal etat : t_etat;

	COMPONENT er_1octet
	PORT(
		rst : IN std_logic;
		clk : IN std_logic;
		en : IN std_logic;
		din : IN std_logic_vector(7 downto 0);
		miso : IN std_logic;          
		sclk : OUT std_logic;
		mosi : OUT std_logic;
		dout : OUT std_logic_vector(7 downto 0);
		busy : OUT std_logic
		);
	END COMPONENT;

	signal din_er : std_logic_vector(7 downto 0);
	signal dout_er : std_logic_vector(7 downto 0);
	signal busy_er : std_logic;
	signal en_er : std_logic;

begin

	Inst_er_1octet: er_1octet PORT MAP(
		rst => rst,
		clk => clk,
		en => en_er,
		din => din_er,
		miso => miso,
		sclk => sclk,
		mosi => mosi,
		dout => dout_er,
		busy => busy_er
	);

	process(clk, rst)
		variable nb_att, num_octet : natural;
		variable r1, r2, r3 : std_logic_vector(7 downto 0);

	begin
		if (rst='0') then
			-- Remise à zéro des signaux et des variables
			-- Signaux
			etat <= idle;
			ss <= '1';
			busy <= '0';
			en_er <= '0';
			din_er <= (others => '0');
			-- Variables
			nb_att := 0;
			num_octet := 0;
			r1 := (others => '0');
			r2 := (others => '0');
			r3 := (others => '0');
		
		elsif(rising_edge(clk)) then
			case etat is
				when idle =>
					if en = '1' then
						-- on initialise toutes les variables et les signaux
						nb_att := 9;
						num_octet := 3;
						r1 := v1;
						r2 := v2;
						r3 := (others => '0');
						busy <= '1';
						ss <= '0';
						etat <= attente;
					end if;

				when attente =>
					-- on attend nb_att cycles
					nb_att := nb_att - 1;
					if nb_att = 0 then
						-- on passe à l'état d'échange
						en_er <= '1';
						etat <= echange;
						case num_octet is 
							when 3 =>
								din_er <= r1;
							when 2 =>
								din_er <= r2;
							when 1 =>
								din_er <= r3;
							when others =>
								din_er <= (others => '0');
						end case;
					end if;
				
				when echange =>
					en_er <= '0';
					-- on attend que l'envoi soit termine
					if busy_er = '0' and en_er = '0' then
						nb_att := 2;
						num_octet := num_octet - 1;
						-- on ecrit les résultats et on passe à l'état de repos
						if num_octet = 0 then
							r3 := dout_er;
							busy <= '0';
							ss <= '1';
							etat <= idle;
							val_and <= r1;
							val_xor <= r2;
							val_nor <= r3;
						-- on echange puis on passe à l'état d'attente
						elsif num_octet = 1 then
							r2 := dout_er;
							etat <= attente;
						-- on echange puis on passe à l'état d'attente
						else
							r1 := dout_er;
							etat <= attente;
						end if;
					end if;
					
				end case;
			end if;
						
	end process;

end behavior;
