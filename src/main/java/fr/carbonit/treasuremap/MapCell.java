package fr.carbonit.treasuremap;

public abstract class MapCell {
    protected final Integer xCoordinate;
    protected final Integer yCoordinate;

    private Integer numberOfTreasuresOnCell = 0;

    protected MapCell(Integer xCoordinate, Integer yCoordinate) {
        this.xCoordinate = xCoordinate;
        this.yCoordinate = yCoordinate;
    }

    public Boolean isMountain() {
        return false;
    }

    public void addTreasure() {
        numberOfTreasuresOnCell++;
    }

    public Integer getTreasures() {
        return numberOfTreasuresOnCell;
    }
}
