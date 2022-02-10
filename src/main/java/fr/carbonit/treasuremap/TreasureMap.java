package fr.carbonit.treasuremap;

public class TreasureMap {
    private final Integer width;
    private final Integer height;

    private MapCell[][] mapCells;

    public TreasureMap(Integer width, Integer height) {
        this.width  = width;
        this.height = height;

        this.mapCells = new MapCell[width][height];

        initializeMapWithPlainCells();
    }

    private void initializeMapWithPlainCells() {
        for (int mapWidth = 0; mapWidth < width; mapWidth++) {
            for (int mapHeight = 0; mapHeight < height; mapHeight++) {
                mapCells[mapWidth][mapHeight] = new PlainCell(mapWidth, mapHeight);
            }
        }
    }

    public void addMountain(Integer xCoordinate, Integer yCoordinate) {
        this.mapCells[xCoordinate][yCoordinate] = new MountainCell(xCoordinate, yCoordinate);
    }

    public String toString() {
        StringBuilder mapDefinition = new StringBuilder("C - " + width + " - " + height + "\n");

        for (int mapWidth = 0; mapWidth < width; mapWidth++) {
            for (int mapHeight = 0; mapHeight < height; mapHeight++) {
                MapCell mapCell = mapCells[mapWidth][mapHeight];
                if (Boolean.TRUE.equals(mapCell.isMountain())) {
                    mapDefinition.append(mapCell)
                                 .append("\n");
                }
            }
        }
        mapDefinition.deleteCharAt(mapDefinition.lastIndexOf("\n"));
        return mapDefinition.toString();
    }
}
