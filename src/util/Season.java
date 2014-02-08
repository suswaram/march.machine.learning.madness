package util;

import java.util.ArrayList;
import java.util.HashMap;

public class Season {
	
	private ArrayList<Game> regGames;
	private HashMap<String, Team> regTeams;
	
	private ArrayList<Game> tourneyGames;
	private HashMap<String, Team> tourneyTeams;
	
	public Season(){
		this.regGames = new ArrayList<Game>();
		this.regTeams = new HashMap<String, Team>();
		this.tourneyGames = new ArrayList<Game>();
		this.tourneyTeams = new HashMap<String, Team>();
	}
	
	/*
	 * Getters
	 */
	
	public ArrayList<Game> getRegGames(){
		return(this.regGames);
	}
	
	public HashMap<String, Team> getRegTeams(){
		return(this.regTeams);
	}
	
	public ArrayList<Game> getTourneyGames(){
		return(this.tourneyGames);
	}
	
	public HashMap<String, Team> getTourneyTeams(){
		return(this.tourneyTeams);
	}
	
	/*
	 * Setters
	 */
	
	public void setRegGames(ArrayList<Game> games){
		this.regGames = games;
	}
	
	public void setRegTeams(HashMap<String, Team> teams){
		this.regTeams = teams;
	}
	
	public void setTourneyGames(ArrayList<Game> games){
		this.tourneyGames = games;
	}
	
	public void setTourneyTeams(HashMap<String, Team> teams){
		this.regTeams = teams;
	}
	
	public void addRegGame(Game gm){
		this.regGames.add(gm);
		
		/* Look to see if we already have team; add it if not */
		Team homeTeam = gm.getHomeTeam();
		Team awayTeam = gm.getAwayTeam();
		if (!this.regTeams.containsKey(awayTeam.getName())){
			this.regTeams.put(awayTeam.getName(), awayTeam);
		}
		
		if (!this.regTeams.containsKey(homeTeam.getName())){
			this.regTeams.put(homeTeam.getName(), homeTeam);
		}
	}

}
