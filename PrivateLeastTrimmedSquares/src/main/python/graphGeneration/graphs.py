import csv
from matplotlib import pyplot as plt

filename = "../../resources/output.csv"
data = []
with open(filename) as f:
    reader = csv.reader(f)
    headerRow = next(reader)
    for row in reader:
        data.append(row)

dataTypes = ["Gaussian", "Bimodal", "Outlier"]
ns = [(0,100),(100, 1000)]
for (n1, n2) in ns:

    for type in dataTypes:
        outlierData = data
        x_mclts = []
        x_ns = []
        y_mclts = []
        y_ns = []
        for d in outlierData:
            if(d[0] == type and int(d[2]) > n1 and int(d[2]) <= n2):
                x_mclts.append(int(d[2]))
                y_mclts.append(float(d[6]))
                x_ns.append(int(d[2]))
                y_ns.append(float(d[9]))



        # plt.scatter(x_ns, y_ns, label="ns", s=1)
        figure, axis = plt.subplots(1, 3)
        figure.tight_layout(h_pad=4)
        p0 = axis[0]
        p0.scatter(x_ns,y_ns, label="NS", s=4, c='red')
        p0.scatter(x_mclts,y_mclts, label="DP-LTS", s=4, c='blue')
        p0.legend()
        p0.set_title("DP-LTS vs NS")
        p1 = axis[1]
        p1.scatter(x_mclts,y_mclts, label="DP-LTS", s=10, c='blue')
        p1.set_title("DP-LTS")
        p2 = axis[2]
        p2.scatter(x_ns,y_ns, label="NS", s=10, c='red')
        p2.set_title("NS")
        figure.suptitle(type + ' for Noisy Stats vs DP-LTS' )
        axis[0].set_ylabel("Error")
        axis[1].set_xlabel("Count")
        plt.subplots_adjust(top=0.85, left=.1, bottom=.1)
        plt.savefig(type + "_" + str(n1)+ "_" + str(n2)+ ".png", dpi=400)


