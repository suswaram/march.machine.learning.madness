package util;

public class Game {
	
	private Team awayTeam;
	private Team homeTeam;
	private int awayScore;
	private int homeScore;
	private boolean neutralLoc;
	private int dayNum;
	
	public Game(Team a, Team b, int awayScore, int homeScore, int dayNum, boolean neutralLoc){
		this.awayTeam = a;
		this.homeTeam = b;
		this.awayScore = awayScore;
		this.homeScore = homeScore;
		this.neutralLoc = neutralLoc;
		this.dayNum = dayNum;
	}
	
	/*
	 * Getters
	 */
	
	public Team getAwayTeam(){
		return(this.awayTeam);
	}
	
	public Team getHomeTeam(){
		return(this.homeTeam);
	}
	
	public int getAwayScore(){
		return(this.awayScore);
	}
	
	public int getHomeScore(){
		return(this.homeScore);
	}
	
	public boolean getNeutralLoc(){
		return(this.neutralLoc);
	}
	
	public int getDayNum(){
		return(this.dayNum);
	}
	
	/*
	 * Setters
	 */

	public void setAwayTeam(Team awayTeam){
		this.awayTeam = awayTeam;
	}
	
	public void setHomeTeam(Team homeTeam){
		this.homeTeam = homeTeam;
	}
	
	public void setAwayScore(int score){
		this.awayScore = score;
	}
	
	public void setHomeScore(int score){
		this.homeScore = score;
	}
	
	public void setNeutralLoc(boolean neutralLoc){
		this.neutralLoc = neutralLoc;
	}
	
	public void setDayNum(int dayNum){
		this.dayNum = dayNum;
	}
}
