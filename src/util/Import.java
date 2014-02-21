package util;

import java.io.*;

public class Import {

	/**
	 * @param args
	 */
	public static Season readRegSeason(String filePath) {
		
		BufferedReader br = null;
		String line, wteamstr, lteamstr, wloc;
		int daynum, wscore, lscore;
		Season season;
		Team wteam, lteam;
		Game gm;
		
		String cvsSplitBy = ",";
		season = new Season();
		
		try {
			
			br = new BufferedReader(new FileReader(filePath));
			while ((line = br.readLine()) != null) {
				String[] game = line.split(cvsSplitBy);
				
				daynum = Integer.parseInt(game[1]);
				wteamstr = game[2];
				wscore = Integer.parseInt(game[3]);
				lteamstr = game[4];
				lscore = Integer.parseInt(game[5]);
				wloc = game[6];
				
				if (season.hasTeam(wteamstr)){
					wteam = season.getTeam(wteamstr);
				} else {
					wteam = new Team(wteamstr);
				}
				
				if (season.hasTeam(lteamstr)){
					lteam = season.getTeam(lteamstr);
				} else {
					lteam = new Team(lteamstr);
				}
				
				if (wloc == "A"){
					gm = new Game(wteam, lteam, wscore, lscore, daynum, wloc == "A");
				} else {
					gm = new Game(lteam, wteam, lscore, wscore, daynum, wloc == "H");
				}
				
				season.addRegGame(gm);
				
			} 
		}catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (br != null) {
				try {
					br.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		return(season);
	}
}
