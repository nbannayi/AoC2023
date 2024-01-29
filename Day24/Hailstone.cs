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

        // Output string representation.
        public override string ToString()
        {
            return $"{_px}, {_py}, {_pz} @ {_vx}, {_vy}, {_vz}";
        }
    }
}