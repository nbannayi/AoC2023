using System;
namespace Day24
{
    /// <summary>
    /// General purpose linear equation solver.
    /// </summary>
    public class LinearSolver
    {
        public double[,] CoefficientsMatrix { get; set; }
        public double[] ValuesVector { get; set; }

        public LinearSolver(double[,] coefficientsMatrix, double[] valuesVector)
        {
            CoefficientsMatrix = coefficientsMatrix;
            ValuesVector = valuesVector;
        }

        // Solve the system - the coefficient matrix will become identity and
        // the solution will be in ValuesVector.  Note: this is crude and doesn't handle
        // invalid inputs or parallel lines.
        public void Solve()
        {
            var n = ValuesVector.Length;
            for (var i = 0; i < n; i++)
            {
                // Get nxn-th element in form 1, a, b, ...
                var pivot = CoefficientsMatrix[i,i];
                for (var j = 0; j < n; j++)                
                    CoefficientsMatrix[i,j] /= pivot;
                ValuesVector[i] /= pivot;
                // Use Gaussian elimination to transform the vectpr.
                for (var i2 = 0; i2 < n; i2++)
                {
                    if (i2 == i) continue;
                    var pivot2 = CoefficientsMatrix[i2,i];
                    for (var j2 = 0; j2 < n; j2++)                                            
                        CoefficientsMatrix[i2,j2] -= pivot2 * CoefficientsMatrix[i,j2];
                    ValuesVector[i2] -= pivot2 * ValuesVector[i];
                }
            }            
        }

        // Show the augmented result matrix.
        public void Display()
        {
            Console.WriteLine("Augmented matrix:");
            for (int i = 0; i <= CoefficientsMatrix.GetUpperBound(0); i++)
            {
                for (int j = 0; j <= CoefficientsMatrix.GetUpperBound(0); j++)                
                    Console.Write($"{CoefficientsMatrix[i,j]}\t");
                Console.WriteLine($" | {ValuesVector[i]}");                
            }
        }
    }
}