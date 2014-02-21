package util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

public abstract class Learning {
	
	public double logistic(double xx){
		return(1/(1+Math.exp(-xx)));
	}

	public void TrainSeason(Season season, int P, double lambda, double gamma, double alpha){
		
		Team homeTm, awayTm;
		double eta, ohat, oo, rraway, rrhome;
		
		ArrayList<Game> games;
		HashMap<String, Team> teams;
		
		for (int p = 0; p < P; p++){
			eta = Math.pow(((1 + 0.1 * P)/(p + 0.1 * P)), .602);
			season.calcNeighborRatings();
			games = season.getRegGames();
			teams = season.getTeams();
			Collections.shuffle(games);
			for (Game gm : games){
				homeTm = teams.get(gm.getHomeTeam().getName());
				awayTm = teams.get(gm.getAwayTeam().getName());
				
				rraway = awayTm.getRating();
				rrhome = homeTm.getRating();
				
				ohat = logistic(rrhome + gamma - rraway);
				oo = logistic(alpha * (gm.getHomeScore() - gm.getAwayScore()));
				
				rrhome = rrhome - eta * (ohat - oo) * ohat * (1 - ohat);
				rraway = rraway + eta * (ohat - oo) * ohat * (1 - ohat);
				
				rrhome = rrhome - eta * (lambda / homeTm.getNumOpp()) * (rrhome - homeTm.getNeighborAvg());
				rraway = rraway - eta * (lambda / awayTm.getNumOpp()) * (rraway - awayTm.getNeighborAvg());
				
				homeTm.setRating(rrhome);
				awayTm.setRating(rraway);
				
				teams.put(awayTm.getName(), awayTm);
				teams.put(homeTm.getName(), homeTm);
				
			}
			season.setRegGames(games);
			season.setTeams(teams);
		}
		
	}
	
}
