----------------------------------------------------------------------
-- Title : SDRAM I/F Sub-Program-Module
--	for (64Mbit x16)
----------------------------------------------------------------------

-- Modded for AS4C32M16SB-6TIN (166MHz) in MISTer SDRAM card
-- 8M word x 16-bit x 4-bank

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

LIBRARY altera_mf;
USE altera_mf.altera_mf_components.all;

entity SDRAM is
	port(
		CLK		: in  std_logic;					-- SDRAM Cycle Clock (57MHz == 17.5ns period)
		nRESET		: in  std_logic;					-- SDRAM I/F RESET

		address		: in  std_logic_vector(24 downto 0);			-- SDRAM Address (32M x16)  Bank2, Row13, Col10
		readdata	: out std_logic_vector(15 downto 0);			-- SDRAM -> main module
		writedata 	: in std_logic_vector(15 downto 0);

		ram_we		: in std_logic;						-- read (0) or write (1)
		trigger		: in std_logic;						-- SDR trigger trg 14MHz one clock pulse
		data_strobe	: in std_logic_vector(1 downto 0);				-- upper/lower data strobe

	-- SDRAM Drive Signal --
		--SDR_CLK		: out std_logic;					-- Drive Clock
		nSDR_CS		: out std_logic;					-- Chip Select
		nSDR_RAS	: out std_logic;					-- Row Active
		nSDR_CAS	: out std_logic;					-- Col Active
		nSDR_WE		: out std_logic;					-- Write Enable
		SDR_DQM		: out std_logic_vector(1 downto 0);			-- Data Request Mask
		SDR_BA		: out std_logic_vector(1 downto 0);			-- Bank Select
		SDR_ADR		: out std_logic_vector(12 downto 0);			-- SDRAM Address Line
		SDR_DAT		: inout std_logic_vector(15 downto 0)			-- SDRAM Data Line (for DirectDrive)
	);
end SDRAM;

architecture RTL of SDRAM is
	type STATE is (RESET,PALL  ,INOP1 ,MRS   ,INOP2 ,				-- Initial State
				IDLE,REF   ,RNOP1 ,RNOP2 ,RNOP3 ,RNOP4 ,RNOP5 ,		-- Refresh State
				WROW  ,WNOP1 ,WCUL  ,WNOP2 ,WNOP3 ,WPRE  , WDMY);	-- Read/Write State
	signal sdrif_state : STATE;

	signal cmd_reg 	 : std_logic_vector(2 downto 0);				-- RAS,CAS,WE
	constant CMD_MRS   : std_logic_vector(2 downto 0) := "000";			-- Mode Register Set
	constant CMD_REF   : std_logic_vector(2 downto 0) := "001";			-- Auto Refresh
	constant CMD_PRE   : std_logic_vector(2 downto 0) := "010";			-- Precharge
	constant CMD_ACT   : std_logic_vector(2 downto 0) := "011";			-- Bank & Row Active
	constant CMD_WRITE : std_logic_vector(2 downto 0) := "100";			-- Write & Colum Select
	constant CMD_READ  : std_logic_vector(2 downto 0) := "101";			-- Read & Column Select
	constant CMD_BST   : std_logic_vector(2 downto 0) := "110";			-- Burst Stop
	constant CMD_NOP   : std_logic_vector(2 downto 0) := "111";			-- No Operation
	constant OP_CODE   : std_logic_vector(12 downto 0):= "0000000100000";		-- CL=2,WT=0,BL=1
																									--         CL2 B BL1
																									-- 00 0 00 010 0 000

	signal timer_count : std_logic_vector(11 downto 0);				-- Refresh & InitWait Timer
	signal refresh_rqst: std_logic;
	signal init_refresh: std_logic_vector(2 downto 0);				-- Initial Refresh Count
	signal init_done   : std_logic;

	signal rddata_reg  : std_logic_vector(15 downto 0);				-- Read Data Buff
	signal wrdata_reg  : std_logic_vector(15 downto 0);				-- Write Data Buff
	signal addr_reg    : std_logic_vector(12 downto 0);
	signal bank_reg    : std_logic_vector(1 downto 0);
	--signal ram_clk		: std_logic_vector(0 downto 0);
	--signal dqm_reg     : std_logic_vector(1 downto 0);
	signal cs_reg      : std_logic;
	signal dout_reg    : std_logic;
	signal trigger_s   : std_logic;

--component altddio_out
--	generic (
--		intended_device_family	:	string := "unused";
--		extend_oe_disable	:	string := "OFF";
--		invert_output		:	string := "OFF";
--		oe_reg			:	string := "UNREGISTERED";
--		power_up_high		:	string := "OFF";
--		width			:	natural;
--		lpm_hint		:	string := "UNUSED";
--		lpm_type		:	string := "altddio_out"
--	);
--	port(
--		aclr		:	in std_logic := '0';
--		aset		:	in std_logic := '0';
--		datain_h	:	in std_logic_vector(width-1 downto 0);
--		datain_l	:	in std_logic_vector(width-1 downto 0);
--		dataout		:	out std_logic_vector(width-1 downto 0);
--		oe		:	in std_logic := '1';
--		oe_out		:	out std_logic_vector(width-1 downto 0);
--		outclock	:	in std_logic;
--		outclocken	:	in std_logic := '1';
--		sclr		:	in std_logic := '0';
--		sset		:	in std_logic := '0'
--	);
--end component;

begin

--==    Concurrent Signal    =======================================--

	nSDR_CS  <= cs_reg;
	nSDR_RAS <= cmd_reg(2);
	nSDR_CAS <= cmd_reg(1);
	nSDR_WE  <= cmd_reg(0);
	SDR_BA   <= bank_reg;
	SDR_ADR  <= addr_reg;
	SDR_DQM  <= addr_reg(12 downto 11);	-- MISTer hack to allow 128MB SDRAM modules. (works with 32, 64, 128MB)
	SDR_DAT  <= wrdata_reg when dout_reg='1' else (others=>'Z');

	readdata   <= rddata_reg;

--==    SDRAM I/F main State     ===================================--

	process (CLK,nRESET) begin
		if (nRESET='0') then
			sdrif_state<= RESET;
			cmd_reg    <= CMD_NOP;
			dout_reg   <= '0';
			cs_reg     <= '0';
			bank_reg   <= "00";
			addr_reg   <= (10=>'1',others=>'X');				-- A10 = 1 forces a precharge all banks in reset block
			--dqm_reg    <= (others=>'1');
			wrdata_reg <= (others=>'X');
			rddata_reg <= (others=>'X');
			trigger_s  <= '0';

		elsif rising_edge(CLK) then
			trigger_s <=trigger;						-- async signal re sampling 

			case sdrif_state is
	-- SDRAM I/F Inital  b izetion state
			when RESET	=>
				if (refresh_rqst = '1') then				--fast time SDR mode set
					sdrif_state<= PALL;
					cmd_reg    <= CMD_PRE;				-- precharge (all since A10 = 1 above)
				end if;
			when PALL	=>
				sdrif_state<= INOP1;
				cmd_reg    <= CMD_NOP;
			when INOP1	=>
				sdrif_state<= MRS;
				cmd_reg    <= CMD_MRS;					-- Mode register set
				addr_reg   <= OP_CODE;
			when MRS	=>						-- tMRD(min)	2 clocks
				sdrif_state<= INOP2;
				cmd_reg    <= CMD_NOP;
			when INOP2	=>
				sdrif_state<= IDLE;

	-- IDLE state
			when IDLE	=>
				if (trigger_s = '1') then
					sdrif_state<= WROW;
					cmd_reg    <= CMD_ACT;				-- Activate bank
					bank_reg   <= address(24 downto 23); 		-- BANK 2 bits
					addr_reg   <= address(22 downto 10);		-- ROW 13 bits
--				elsif (init_done='0' or refresh_rqst = '1') then
--					sdrif_state<= REF;
--					cmd_reg    <= CMD_REF;
				end if;

		-- SDRAM (WORD_WRITE or WORD_READ) state
			when WROW	=>						-- tRCD(min) 18ns   /RAS to /CAS delay
				sdrif_state<= WNOP1;
				cmd_reg    <= CMD_NOP;
				--dqm_reg <= (others=>'0');
			when WNOP1	=>
				sdrif_state<= WCUL;
				if (ram_we='1') then
					cmd_reg  <= CMD_WRITE;
					addr_reg   <= not data_strobe & '0' & address(9 downto 0);	-- A12:11 = L/UDQM on write , A10=0 (AUTO PRECHARGE OFF)     COLUMN 10 bits
					--dqm_reg <= data_strobe;				-- upper/lower byte selection
					wrdata_reg <= writedata;			-- lower/upper same data
					dout_reg <='1';					-- output data on next clock edge
				else
					addr_reg   <= "000" & address(9 downto 0);		-- A10=0 (AUTO PRECHARGE OFF)     COLUMN 10 bits
					cmd_reg  <= CMD_READ;
				end if;
			when WCUL	=>
				sdrif_state<= WNOP2;
				cmd_reg    <= CMD_NOP;
				dout_reg <='0';						-- turn off data out 
			when WNOP2	=>
				sdrif_state<= WNOP3;
				cmd_reg    <= CMD_NOP;
			when WNOP3	=>
				sdrif_state<= WPRE;
				cmd_reg    <= CMD_PRE;					-- finish with a precharge
				if (ram_we='0') then
					rddata_reg <= SDR_DAT;				-- read data available after 2 clocks (CAS Latency = 2)
				end if;
			when WPRE	=>
				sdrif_state<= WDMY;
				cmd_reg    <= CMD_NOP;
			when WDMY   =>							-- only initiate refresh cycle after a write completes
				if (init_done='0' or refresh_rqst = '1') then
					sdrif_state<= REF;
					cmd_reg    <= CMD_REF;				-- Auto refresh
				else
					sdrif_state<= IDLE;
					cmd_reg    <= CMD_NOP;
				end if;
				
	-- SDRAM Refresh state						tARFC(min)	60ns on -6 chip.
			when REF	=>
				sdrif_state<= RNOP1;
				cmd_reg   <= CMD_NOP;
			when RNOP1	=>
				sdrif_state<= RNOP2;
			when RNOP2	=>
				sdrif_state<= RNOP3;
			when RNOP3	=>
				sdrif_state<= RNOP4;
			when RNOP4	=>
				sdrif_state<= RNOP5;
			when RNOP5	=>
				sdrif_state<= IDLE;

			when others	=> NULL;
			end case;

		end if;
	end process;

--==  SDRAM Initialize/Refresh State  ==============================--

	process (CLK,nRESET) begin
		if (nRESET='0') then
			timer_count  <= CONV_std_logic_vector(1,12);
			refresh_rqst <= '0';
			init_refresh <= "000";
			init_done    <= '0';
		elsif rising_edge(CLK) then
			if (refresh_rqst = '1') then
				if (sdrif_state = REF) then
					refresh_rqst <= '0';
				end if;
			end if;

			if (timer_count = 0) then
				refresh_rqst <= '1';
				timer_count <= CONV_std_logic_vector(445,12);		-- 7.805uS for 57MHz   (17.54ns * 445)
			else								-- 8192 * 7.805us = 64084us = 63.941ms which meets 64ms requirement
				timer_count <= timer_count - 1;
			end if;

			if (sdrif_state = REF) then
				if (init_refresh = "111") then				-- force 8 refresh cycles @ power-on or reset?
					init_done <= '1';
				else
					init_refresh <= init_refresh + 1;
				end if;
			end if;

		end if;
	end process;

--ado: altddio_out
--	generic map (
--	extend_oe_disable	=>	"OFF",
--	intended_device_family	=>	"Cyclone V",
--	invert_output		=>	"OFF",
--	lpm_hint		=>	"UNUSED",
--	lpm_type		=>	"altddio_out",
--	oe_reg			=>	"UNREGISTERED",
--	power_up_high		=>	"OFF",
--	width			=>	1
--	)
--	port map(
--	datain_h	=>	"0",
--	datain_l	=>	"1",
--	outclock	=>	CLK,
--	dataout		=>	ram_clk,
--	aclr		=>	'0',
--	aset		=>	'0',
--	oe		=>	'1',
--	outclocken	=>	'1',
--	sclr		=>	'0',
--	sset		=>	'0'
--	);


end RTL;

