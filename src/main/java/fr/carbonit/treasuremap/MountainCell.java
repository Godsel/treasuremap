package fr.carbonit.treasuremap;

public class MountainCell
        extends MapCell {
    public MountainCell(Integer xCoordinate, Integer yCoordinate) {
        super(xCoordinate, yCoordinate);
    }

    public String toString() {
        return "M - " + xCoordinate + " - " + yCoordinate;
    }

    @Override
    public Boolean isMountain() {
        return true;
    }
}
