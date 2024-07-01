using LambdaMan.Map;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Linq;

namespace LambdaMan.Gameloop
{
    internal class DijkstraFinder
    {
        public List<Tile> GetShortestPathDijkstra(Tile start, Tile end)
        {
            DijkstraSearch(start, end);
            var shortestPath = new List<Tile>();
            shortestPath.Add(end);
            BuildShortestPath(shortestPath, end);
            if(shortestPath.Count == 1) {
                return new List<Tile>();
            }
            shortestPath.Reverse();
            return shortestPath;
        }

        private void BuildShortestPath(List<Tile> list, Tile node)
        {
            if (node.NearestToStart == null)
                return;
            list.Add(node.NearestToStart);
            BuildShortestPath(list, node.NearestToStart);
        }

        private void DijkstraSearch(Tile Start, Tile End)
        {
            Start.MinCostToStart = 0;
            var prioQueue = new List<Tile>();
            prioQueue.Add(Start);
            do
            {
                prioQueue = prioQueue.OrderBy(x => x.MinCostToStart).ToList();
                var node = prioQueue.First();
                prioQueue.Remove(node);
                foreach (var cnn in node.Connections.Where(x => x.isWalkable).OrderBy(x => x.Cost))
                {
                    var childNode = cnn;
                    if (childNode.Visited)
                        continue;
                    if (childNode.MinCostToStart == null ||
                        node.MinCostToStart + cnn.Cost < childNode.MinCostToStart)
                    {
                        childNode.MinCostToStart = node.MinCostToStart + cnn.Cost;
                        childNode.NearestToStart = node;
                        if (!prioQueue.Contains(childNode))
                            prioQueue.Add(childNode);
                    }
                }
                node.Visited = true;
                if (node == End)
                    return;
            } while (prioQueue.Any());
        }
    }
}
