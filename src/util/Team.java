package util;

public class Team {
	
	private String name;
	private Double eloRating;
	private int numNeighbors;
	private Double neighborSum;
	
	public Team(String name){
		this.name = name;
		this.eloRating = 0.0;
		this.numNeighbors = 0;
		this.neighborSum = 0.0;
	}
	
	/*
	 * Getters
	 */
	public String getName(){
		return(this.name);
	}
	
	public Double getRating(){
		return(this.eloRating);
	}
	
	public Double getNeighborAvg(){
		return(this.neighborSum/this.numNeighbors);
	}
	
	public int getNumOpp(){
		return(this.numNeighbors);
	}
	
	/*
	 * Setters
	 */
	public void setName(String name){
		this.name = name;
	}
	
	public void setRating(Double rating){
		this.eloRating = rating;
	}
	
	public void resetRating(){
		this.eloRating = 0.0;
	}
	
	public void resetOppRatings(){
		this.neighborSum = 0.0;
		this.numNeighbors = 0;
	}
	
	public void addOppo(Team tm){
		this.neighborSum += tm.getRating();
		this.neighborSum++;
	}
	

}
