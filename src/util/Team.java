package util;

public class Team {
	
	private String name;
	private Double eloRating;
	private int numNeighbors;
	private Double neighborSum;
	
	public Team(String name){
		this.name = name;
		this.eloRating = 0.0;
		numNeighbors = 0;
		neighborSum = 0.0;
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
	
	/*
	 * Setters
	 */
	public void setName(String name){
		this.name = name;
	}
	
	public void setRating(Double rating){
		this.eloRating = rating;
	}
	

}
