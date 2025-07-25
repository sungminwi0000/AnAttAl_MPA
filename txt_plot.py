import matplotlib.pyplot as plt
import sys
import os

def Kraft_oder_Strom_data_lesen(file_path):
    Zeit_list = []
    Zahl_list = []
    with open(file_path, 'r') as file:
        for line in file:
            if line.strip() and not line.startswith('END'):
                parts = line.strip().split(',')
                if len(parts) >= 2:
                    try:
                        Zeit = float(parts[0])
                        Zahl = float(parts[1])
                        Zeit_list.append(Zeit)
                        Zahl_list.append(Zahl)
                    except ValueError:
                        continue
    return Zeit_list, Zahl_list

def Potentialverlauf_data_lesen(file_path):
    Zeit_list = []
    Zahl_1_list = []
    Zahl_2_list = []
    Zahl_3_list = []
    with open(file_path, 'r') as file:
        for line in file:
            if line.strip() and not line.startswith('END'):
                parts = [p.strip() for p in line.strip().split(',')]
                if len(parts) >= 4:  # 至少4列数据
                    try:
                        Zeit = float(parts[0])
                        Zahl_1 = float(parts[1])
                        Zahl_2 = float(parts[2])
                        Zahl_3 = float(parts[3])

                        Zeit_list.append(Zeit)
                        Zahl_1_list.append(Zahl_1)
                        Zahl_2_list.append(Zahl_2)
                        Zahl_3_list.append(Zahl_3)
                    except ValueError:
                        continue

    return Zeit_list, Zahl_1_list, Zahl_2_list, Zahl_3_list

def plot_Kraft_oder_Strom_data(time, values, title, ylabel, filename):
    plt.figure(figsize=(10, 6))
    plt.plot(time, values)
    plt.title(title)
    plt.xlabel('Zeit [ms]')
    plt.ylabel(ylabel)
    plt.grid(True)
    plt.savefig(filename)
    # plt.close()

def plot_Potentialverlauf_data(time, values1, values2, values3, filename):
    plt.figure(figsize=(10, 6))
    plt.plot(time, values1, label='Potentialverlauf EB1')
    plt.plot(time, values2, label='Potential BB')
    plt.plot(time, values3, label='Potential EB2')
    plt.title('Potentialverlauf')
    plt.xlabel('Zeit [ms]')
    plt.ylabel('Potential [V]')
    plt.legend()
    plt.grid(True)
    plt.savefig(filename)
    # plt.close()

def main():
    # 获取脚本所在目录
    script_dir = os.path.dirname(os.path.abspath(__file__))
    
    # 定义固定文件名
    kraft_file = os.path.join(script_dir, 'Kraftverlauf.txt')
    potential_file = os.path.join(script_dir, 'Potentialverlauf.txt')
    strom_file = os.path.join(script_dir, 'Stromverlauf.txt')

    # 检查文件是否存在
    if not all(os.path.exists(f) for f in [kraft_file, potential_file, strom_file]):
        print("Error: One or more data files are missing in the script directory.")
        print("Please ensure the following files exist:")
        print("- Kraftverlauf.txt")
        print("- Potentialverlauf.txt")
        print("- Stromverlauf.txt")
        sys.exit(1)

    # 读取并绘制Kraftverlauf数据
    kraft_time, kraft_values = Kraft_oder_Strom_data_lesen(kraft_file)
    plot_Kraft_oder_Strom_data(kraft_time, kraft_values, 'Kraftverlauf', 'Kraft [kN]', 'Kraftverlauf.png')

    # 读取并绘制Potentialverlauf数据
    pot_time, pot1, pot2, pot3 = Potentialverlauf_data_lesen(potential_file)
    plot_Potentialverlauf_data(pot_time, pot1, pot2, pot3, 'Potentialverlauf.png')

    # 读取并绘制Stromverlauf数据
    strom_time, strom_values = Kraft_oder_Strom_data_lesen(strom_file)
    plot_Kraft_oder_Strom_data(strom_time, strom_values, 'Stromverlauf', 'Strom [A]', 'Stromverlauf.png')

    print("Plots saved as Kraftverlauf.png, Potentialverlauf.png, and Stromverlauf.png")

if __name__ == "__main__":
    import sys 
    import matplotlib.pyplot as plt
    import os
    main()
