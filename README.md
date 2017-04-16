using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Test1
{
    class PatientException : Exception
    {
        public string s;
        public PatientException(string h)
            : base(h)
        {  s = h;  }
    }
    class SelectionException : Exception
    {
        public string s;
        public SelectionException(string h)
            : base(h)
        { s = h; }
    }
    class ModeException : Exception
    {
        public string s;
        public ModeException(string h)
            : base(h)
        { s = h; }
    }
    class PercentException : Exception
    {
        public string s;
        public PercentException(string h)
            : base(h)
        { s = h; }
    }

    class Matrix
    {
        public static object[] Analiz(int mode, double percent, double[,] selection, double[] outcomes, int[] identifiers, double[] patient)
        {
            object[] rezultat = new object[1]; ;
            if (mode != 0 && mode != 1) throw new ModeException("Ошибка ввода данных о режиме работы.");
            if (selection.GetLength(0) != outcomes.GetLength(0) || selection.GetLength(0) != identifiers.GetLength(0) || selection.GetLength(1) != patient.GetLength(0)) throw new SelectionException("Ошибка ввода данных о обучающей выборке.");
            if (selection.GetLength(1) != patient.GetLength(0)) throw new PatientException("Ошибка ввода данных о пациенте.");
            int r = 10;
            double proсN = 0;
            double procT = 0;
            double N = 0;
            double C = 0;
            double[] R;
            for (; ; )
            {
                int o = 0;
                double[] oshib = new double[selection.GetLength(0)];
                double[] rez = new double[selection.GetLength(0)];
                R = Nachaln(selection, outcomes, r);
                for (int j = 1; j < rez.GetLength(0); ++j)
                {
                    double[] p1 = new double[selection.GetLength(1)];
                    for (int i = 1; i < selection.GetLength(1); ++i) p1[i] = selection[j, i];
                    double w = R[0];
                    double z = 0;
                    for (int i = 1; i < R.GetLength(0); ++i) w += p1[i] * R[i];
                    for (int i = 1; i < R.GetLength(0); ++i) z += Math.Pow(R[i], 2);
                    rez[j] = w / Math.Sqrt(z);
                }

                proсN = C / N;
                N = 0;
                C = 0;

                for (int j = 1; j < rez.GetLength(0); ++j)
                {
                    ++N;
                    if ((rez[j] > 0 && outcomes[j] > 0) || (rez[j] < 0 && outcomes[j] < 0)) ++C;
                    else
                    {
                        oshib[j] = 1;
                        o++;
                    }
                }

                procT = C / N;
                if (procT - proсN < 0.01)
                {
                    if (mode == 0)
                    {
                        rezultat = new object[o + 1];
                        rezultat[0] = Math.Round(procT, 2);
                        int v = 1;
                        for (int i = 0; i < oshib.GetLength(0); ++i)
                            if (oshib[i] == 1)
                            {
                                rezultat[v] = identifiers[i];
                                ++v;
                            }
                        return rezultat;
                    }
                    else break;
                }
                else { r = r * 2; }
            }
            if (mode == 1)
            {
                if (procT < percent)
                {
                    throw new PercentException("Не обеспечена заданная точность прогноза.");
                }
                double h = 0;
                double sum = 0;
                rezultat = new object[2];
                for (int j = 0; j < patient.GetLength(0); ++j)
                {
                    sum += R[j] * patient[j];
                }
                h = 1.0 / (1.0 + Math.Exp(-sum));
                rezultat[0] = Math.Round(h, 2);
                rezultat[1] = Math.Round(1 - h, 2);
            }
            return rezultat;
        }


        public static double[] Nachaln(double[,] F, double[] y, double q)
        {
            double[] O;
            double h = 0.5;
            double e = 0.0000000001;
            int maxI = 0;
            bool a = false;
            bool b = false;
            O = Mnk(F, y);
            for (int i = 0; i < O.GetLength(0); ++i) if (Math.Abs(O[i]) > O[maxI]) maxI = i;
            double r = q / O[maxI];
            for (int i = 0; i < O.GetLength(0); ++i) O[i] = O[i] * r;
            for (int t = 1; ; ++t)
            {
                double Q = 0;
                double Qt = 0;
                if (!a) h = h * 0.5;
                b = false;
                for (int g = O.GetLength(0) - 1; g >= 0; --g)
                {
                    a = false;
                    if (maxI != g)
                    {
                        Q = 0;
                        Qt = 0;
                        double ot = O[g] + h;
                        for (int i = 0; i < F.GetLength(0); ++i)
                        {
                            double sum = 0;
                            for (int j = 0; j < O.GetLength(0); ++j) sum += O[j] * F[i, j];
                            Q += Math.Log(1 + Math.Exp(-y[i] * sum));
                        }
                        for (int i = 0; i < F.GetLength(0); ++i)
                        {
                            double sum = 0;
                            for (int j = 0; j < O.GetLength(0); ++j)
                            {
                                if (j != g) sum += O[j] * F[i, j];
                                else sum += ot * F[i, j];
                            }
                            Qt += Math.Log(1 + Math.Exp(-y[i] * sum));

                        }
                        if (Qt < Q)
                        {
                            a = true;
                            b = true;
                            O[g] += h;
                        }
                        if (!b)
                        {
                            Q = 0;
                            ot = O[g] - h;
                            Qt = 0;
                            for (int i = 0; i < F.GetLength(0); ++i)
                            {
                                double sum = 0;
                                for (int j = 0; j < O.GetLength(0); ++j)
                                {
                                    sum += O[j] * F[i, j];
                                }
                                Q += Math.Log(1 + Math.Exp(-y[i] * sum));
                            }
                            for (int i = 0; i < F.GetLength(0); ++i)
                            {
                                double sum = 0;
                                for (int j = 0; j < O.GetLength(0); ++j)
                                {
                                    if (j != g) sum += O[j] * F[i, j];
                                    else sum += ot * F[i, j];
                                }
                                Qt += Math.Log(1 + Math.Exp(-y[i] * sum));
                            }
                            if (Qt < Q)
                            {
                                a = true;
                                O[g] -= h;
                            }
                        }
                    }
                }
                if (Math.Abs(Qt - Q) < e) { return O; }
            }
        }

        public static double[,] T(double[,] X)
        {
            double[,] X_dop = new double[X.GetLength(1), X.GetLength(0)];
            for (int i = 0; i < X.GetLength(1); i++)
                for (int j = 0; j < X.GetLength(0); j++)
                    X_dop[i, j] = X[j, i];
            return X_dop;
        }

        public static double[,] inversion(double[,] A)
        {
            int N = A.GetLength(0);
            double[,] E = new double[N, N];
            double temp;
            for (int i = 0; i < N; i++) for (int j = 0; j < N; j++) E[i, j] = 0;
            for (int i = 0; i < N; i++)
                for (int j = 0; j < N; j++)
                {
                    E[i, j] = 0.0;
                    if (i == j) E[i, j] = 1.0;
                }
            for (int k = 0; k < N; k++)
            {
                temp = A[k, k];
                for (int j = 0; j < N; j++)
                {
                    A[k, j] /= temp;
                    E[k, j] /= temp;
                }
                for (int i = k + 1; i < N; i++)
                {
                    temp = A[i, k];
                    for (int j = 0; j < N; j++)
                    {
                        A[i, j] -= A[k, j] * temp;
                        E[i, j] -= E[k, j] * temp;
                    }
                }
            }
            for (int k = N - 1; k > 0; k--)
            {
                for (int i = k - 1; i >= 0; i--)
                {
                    temp = A[i, k];
                    for (int j = 0; j < N; j++)
                    {
                        A[i, j] -= A[k, j] * temp;
                        E[i, j] -= E[k, j] * temp;
                    }
                }
            }
            return E;
        }

        static double[,] MultiplicationM(double[,] a, double[,] b)
        {
            double[,] r = new double[a.GetLength(0), b.GetLength(1)];
            for (int i = 0; i < a.GetLength(0); i++)
                for (int j = 0; j < b.GetLength(1); j++)
                    for (int k = 0; k < b.GetLength(0); k++)
                        r[i, j] += a[i, k] * b[k, j];
            return r;
        }
        static double[] MultiplicationMV(double[,] a, double[] b)
        {
            double[] r = new double[a.GetLength(0)];
            for (int i = 0; i < a.GetLength(0); i++)
                for (int j = 0; j < b.GetLength(0); j++)
                    r[i] += a[i, j] * b[j];
            return r;
        }

        public static double[] Mnk(double[,] Fi, double[] yt)
        {
            double[,] T1 = T(Fi);
            double[,] I1 = inversion(MultiplicationM(T1, Fi));
            double[,] M2 = MultiplicationM(I1, T1);
            double[] G = MultiplicationMV(M2, yt);
            return G;
        }
    }
}
