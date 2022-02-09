package fr.carbonit.treasuremap;

public class TreasureMap {
    private final Integer width;
    private final Integer height;

    public TreasureMap(Integer width, Integer height) {
        this.width = width;
        this.height = height;
    }

    public String toString() {
        return "C - " + width + " - " + height;
    }
}
