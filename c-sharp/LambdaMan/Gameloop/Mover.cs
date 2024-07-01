using LambdaMan.Map;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LambdaMan.Gameloop
{
    internal class Mover
    {
        private List<Tile> tiles;
        public Mover(List<Tile> parsedTiles) 
        {
            this.tiles = parsedTiles;
        }
        public void Loop()
        {
            var startingPoint = tiles.First(x => x.isPlayer);
            while(tiles.Any(x => x.hasPill))
            {
               var pathToTake = GetPathTarget(startingPoint);

                foreach(var step in pathToTake)
                {
                    Move(step);
                    startingPoint = step;
                }

                int pillCount = 0;

                //fix costs
                foreach(var tile in tiles)
                {
                    tile.FixCosts();
                    tile.MinCostToStart = null;
                    tile.NearestToStart = null;
                    tile.Visited = false;
                    if (tile.hasPill)
                    {
                        pillCount = pillCount + 1;
                    }
                }
                Console.WriteLine($"Remaining pills on board: {pillCount}");
            }
        }

        public void Move(Tile source)
        {
            if(source != null)
            {
                source.hasPill = false;
                path.Push(source);
            }
        }

        private Stack<Tile> path = new Stack<Tile>();

        public List<Tile> GetPathTarget(Tile source)
        {
            if (source == null)
                return null;

            DijkstraFinder finder = new DijkstraFinder();
            var shortestPath = new List<Tile>();
            foreach (Tile tile in tiles.Where(x => x.hasPill))
            {
                var tempPath = finder.GetShortestPathDijkstra(source, tile);
                //skip invalid paths
                if (tempPath.Count == 0)
                {
                    continue;
                }
                var tempCost = tempPath.Sum(x => x.Cost);
                if(shortestPath.Count == 0)
                {
                    shortestPath = tempPath;
                }
                else if (shortestPath.Sum(x => x.Cost) > tempCost)
                {
                     shortestPath = tempPath;
                }
            }
            return shortestPath;
        }
        public Stack<Tile> GetPath()
        {
            return path;
        }
    }
}
