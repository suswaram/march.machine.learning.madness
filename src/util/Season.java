package util;

import java.util.ArrayList;
import java.util.HashMap;

public class Season {
	
	private ArrayList<Game> regGames;
	private HashMap<String, Team> teams;
	
	private ArrayList<Game> tourneyGames;
	
	public Season(){
		this.regGames = new ArrayList<Game>();
		this.teams = new HashMap<String, Team>();
		this.tourneyGames = new ArrayList<Game>();
	}
	
	/*
	 * Getters
	 */
	
	public ArrayList<Game> getRegGames(){
		return(this.regGames);
	}
	
	public HashMap<String, Team> getTeams(){
		return(this.teams);
	}
	
	public ArrayList<Game> getTourneyGames(){
		return(this.tourneyGames);
	}
	
	public Team getTeam(String name){
		return(this.teams.get(name));
	}

	
	/*
	 * Setters
	 */
	
	public void setRegGames(ArrayList<Game> games){
		this.regGames = games;
	}
	
	public void setTeams(HashMap<String, Team> teams){
		this.teams = teams;
	}
	
	public void setTourneyGames(ArrayList<Game> games){
		this.tourneyGames = games;
	}
	
	public void addRegGame(Game gm){
		this.regGames.add(gm);
		
		/* Look to see if we already have team; add it if not */
		Team homeTeam = gm.getHomeTeam();
		Team awayTeam = gm.getAwayTeam();
		
		if (!this.teams.containsKey(awayTeam.getName())){
			this.teams.put(awayTeam.getName(), awayTeam);
		}
		
		if (!this.teams.containsKey(homeTeam.getName())){
			this.teams.put(homeTeam.getName(), homeTeam);
		}
	}
	
	public boolean hasTeam(String name){
		return(teams.containsKey(name));
	}
	
	public void resetRatings(){
		
		for (Team tm : this.teams.values()){
			tm.resetRating();
			this.teams.put(tm.getName(), tm);
		}
		
	}
	
	public void resetOppRatings(){
		
		for (Team tm : this.teams.values()){
			tm.resetOppRatings();
			this.teams.put(tm.getName(), tm);
		}
	}
	
	public void calcNeighborRatings(){
		
		Team awayTm, homeTm;
		resetOppRatings();
		
		for (Game gm : this.regGames){
			
			awayTm = this.teams.get(gm.getAwayTeam().getName());
			homeTm = this.teams.get(gm.getHomeTeam().getName());
			
			awayTm.addOppo(homeTm);
			homeTm.addOppo(awayTm);
			
			this.teams.put(awayTm.getName(), awayTm);
			this.teams.put(homeTm.getName(), homeTm);
		}
	}

}
