package fr.carbonit.treasuremap.cell;

import java.awt.*;

public abstract class MapCell {
    protected Point   coordinates;
    private   Boolean occupiedByAdventurer    = false;
    private   Integer numberOfTreasuresOnCell = 0;

    protected MapCell(Integer xCoordinate, Integer yCoordinate) {
        this.coordinates = new Point(xCoordinate, yCoordinate);
    }

    public Boolean isMountain() {
        return false;
    }

    public void addTreasure() {
        numberOfTreasuresOnCell++;
    }

    public void removeTreasure() {
        numberOfTreasuresOnCell--;
    }

    public Integer getTreasures() {
        return numberOfTreasuresOnCell;
    }

    public void updateAdventurerStatus() {
        occupiedByAdventurer = !occupiedByAdventurer;
    }

    public Boolean isOccupiedByAnAdventurer() {
        return occupiedByAdventurer;
    }
}
