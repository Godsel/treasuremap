package fr.carbonit.treasuremap;

public abstract class MapCell {
    protected final Integer xCoordinate;
    protected final Integer yCoordinate;

    protected MapCell(Integer xCoordinate, Integer yCoordinate) {
        this.xCoordinate = xCoordinate;
        this.yCoordinate = yCoordinate;
    }

    public Boolean isMountain() {
        return false;
    }
}
