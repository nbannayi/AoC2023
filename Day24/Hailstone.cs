using System;
namespace Day24
{
    /// <summary>
    /// Represents a hailstone.
    /// </summary>
    public class Hailstone
    {
        private long _px, _py, _pz, _vx, _vy, _vz;
        private double _gradient, _intercept;
        public double Gradient { get { return _gradient; } }
        public double Intercept { get { return _intercept; } }

        // Instantiate a hailstone object.
        public Hailstone(long px, long py, long pz, long vx, long vy, long vz)
        {
            _px = px; _py = py; _pz = pz; _vx = vx; _vy = vy; _vz = vz;
            _gradient = (double)_vy / _vx;
            _intercept = _py - _gradient * px;
        }

        // Get intersection between this and passed hailstone.
        public (double, double) GetIntersection(Hailstone other)
        {
            var m1 = _gradient;
            var c1 = _intercept;
            var m2 = other.Gradient;
            var c2 = other.Intercept;
            var xi = (c2 - c1) / (m1 - m2);
            var yi = m1 * xi + c1;
            return (xi, yi);
        }

        // Determine if passed hailstone intersects with this one within given bounds.
        public bool IntersectsWithinBounds(Hailstone other, (long,long) bounds)
        {
            var (ix,iy) = GetIntersection(other);
            var (b1, b2) = bounds;
            var boundsCheck = ix >= b1 && ix <= b2 && iy >= b1 && iy <= b2;
            // Only check if in future if bounds check works.
            if (boundsCheck)
            {
                var futureCheckThis =
                    Math.Sign(ix - _px) == Math.Sign(_vx) &&
                    Math.Sign(iy - _py) == Math.Sign(_vy);
                var futureCheckOther =
                    Math.Sign(ix - other._px) == Math.Sign(other._vx) &&
                    Math.Sign(iy - other._py) == Math.Sign(other._vy);
                return futureCheckThis && futureCheckOther;
            }
            return false;
        }

        // Slightly hard to explain but basically takes the current hailstone and another
        // and creates a linear constraint that can be solved as a system of equations.
        // Pass in xy as true for x,y equations, otherise y,z is used.  Then 4 of these can be solved
        // for 4 unknowns in the Solver using GE.  Had to trawl Reddit for inspiration on this one:
        // https://www.reddit.com/r/adventofcode/comments/18q40he/2023_day_24_part_2_a_straightforward_nonsolver/
        public double[] GetLinearConstraint(Hailstone other, bool xy)
        {
            if (xy)
            {
                // Coefficients a,b,c,d,e of a*px + b*py + c*vx + d*vy = e                
                return new double[] {
                    other._vy - _vy,
                    _vx - other._vx,
                    _py - other._py,
                    other._px - _px,
                    other._px * other._vy - other._py * other._vx - _px * _vy + _py * _vx
                };
            }
            else
            {
                // Coefficients a,b,c,d,e of a*py + b*pz + c*vy + d*vz = e                
                return new double[] {
                    other._vz - _vz,
                    _vy - other._vy,
                    _pz - other._pz,
                    other._py - _py,
                    other._py * other._vz - other._pz * other._vy - _py * _vz + _pz * _vy
                };
            }                        
        }

        // Output string representation.
        public override string ToString()
        {
            return $"{_px}, {_py}, {_pz} @ {_vx}, {_vy}, {_vz}";
        }
    }
}