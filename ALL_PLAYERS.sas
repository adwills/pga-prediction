LIBNAME GOLF "/Local_Files/OneDrive - SAS/Projects/Golf/Data";

/*************************CREATE LIST OF ALL PGA PLAYERS*************************/
FILENAME PLAYERS TEMP;

PROC HTTP 
	URL="https://statdata.pgatour.com/players/player.json"
	METHOD= "GET"
	OUT=PLAYERS;
RUN;

LIBNAME PLAYERS JSON FILEREF=PLAYERS;

DATA GOLF.ALL_PLAYERS;
	SET PLAYERS.PLRS (KEEP=ORDINAL_PLRS NAMEL NAMEF NAMEMI PID);
RUN;