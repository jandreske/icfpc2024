using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace LambdaMan.Map
{
    internal class Tile
    {
        public int x, y;

        public Tile? Left;
        public Tile? Right;
        public Tile? Top;
        public Tile? Bottom;
        public bool isWalkable = true;
        public bool hasPill
        {
            get => hasPill1; 
            set
            {
                hasPill1 = value;
            }
        }
        public bool isPlayer = false;
        public char assignedSign;


        public List<Tile> Connections = new List<Tile>();
        public int Cost = 1;
        private bool hasPill1;

        public Tile NearestToStart { get; set; }
        public int? MinCostToStart { get;  set; }
        public bool Visited { get; set; } = false;
        public Tile ConnectedNode { get; set; }

        public Tile(int rowIndex, int characterIndex, char sign)
        {
            assignedSign = sign;
            x = rowIndex;
            y = characterIndex;
            switch (sign)
            {
                case '#':
                    isWalkable = false;
                    Cost = 99999;
                    break;
                case '.':
                    hasPill = true; 
                    break;
                case 'L':
                    isPlayer = true;
                    hasPill = true;
                    break;
            }
        }

        internal void FixCosts()
        {
            if (hasPill)
            {
                Cost = 1;
            }else if(!hasPill && isWalkable)
            {
                Cost = 1000;
            }
            else if (!isWalkable)
            {
                Cost = 999999;
            }
        }
    }
}
