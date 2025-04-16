# Đọc dữ liệu từ file CSV có tên "All_GPUs.csv"
All_GPUs <- read.csv("~/archive/All_GPUs.csv")
# Hiển thị 10 dòng đầu tiên của dữ liệu đã đọc
head(All_GPUs, 10)

# Thay thế tất cả các ô trống trong dữ liệu GPU_data bằng NA
All_GPUs[All_GPUs == ""] <- NA
# Thay thế các giá trị có định dạng "\n-" bằng NA trong tất cả các cột của GPU_data
All_GPUs[] <- lapply(All_GPUs, function(x) gsub("^\\n- $", NA, x))
# Thay thế các chuỗi "NA" bằng NA
All_GPUs[All_GPUs == "NA"] <- NA
# Hiển thị 6 dòng đầu tiên của dữ liệu đã được kiểm tra và thay thế bằng NA 
head(All_GPUs)

# Tạo bảng thống kê số lượng và tỷ lệ dữ liệu khuyết
na_summary <- data.frame(
  Column = names(All_GPUs),
  NA_Count = sapply(All_GPUs, function(x) sum(is.na(x))),
  NA_Percentage = sapply(All_GPUs, function(x) mean(is.na(x)) * 100)
)
# Hiển thị bảng thống kê
library(questionr)
freq.na(All_GPUs)

# Gọi thư viện ggplot2 để sử dụng hàm ggplot
library(ggplot2)
# Tạo biểu đồ cột cho tỷ lệ dữ liệu khuyết ở các biến
ggplot(na_summary, aes(x = Column, y = NA_Percentage, fill = NA_Percentage)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + # Đổi màu gradient
  geom_text(aes(label = paste0(round(NA_Percentage, 1), "%")), 
            vjust = -0.5, size = 1.5 ) +
  labs(title = "Tỷ lệ dữ liệu khuyết ở các biến",
       x = "Biến Dữ Liệu ",
       y = "Tỷ lệ dữ liệu khuyết (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1)) 

# Lọc các cột có tỷ lệ dữ liệu khuyết dưới 15%
selected_columns <- na_summary$Column[na_summary$NA_Percentage < 15 ]
# Giữ lại các cột thỏa mãn điều kiện trong dataframe ban đầu
new_GPU_data <- All_GPUs[, selected_columns]
#Xoá các quan sát chứa dữ liệu khuyết
new_GPU_data<-na.omit(new_GPU_data)

# Lựa chọn các biến cần xoá đơn vị
columns_to_clean <- c("L2_Cache","Memory_Bandwidth","Memory_Bus","Memory_Speed", "Memory", "Process" )
#Tạo hàm thực hiện xoá đơn vị ở các biến 
remove_units <- function(column) {
  # Sử dụng gsub để xóa tất cả các ký tự không phải số (kể cả đơn vị)
  cleaned_column <- gsub("[^0-9.]", "", column)
  # Chuyển đổi kết quả về kiểu numeric
  cleaned_column <- as.numeric(cleaned_column)
  
  return(cleaned_column)
}
# Áp dụng hàm cho các biến đã chọn
new_GPU_data[columns_to_clean] <- lapply(new_GPU_data[columns_to_clean], remove_units)
head(new_GPU_data, 10)

#In ra bảng dữ liệu với các biến cần thiết và hoàn chỉnh sau khi làm sạch dữ liệliệu
main_GPU_data<-new_GPU_data[c("Memory_Bandwidth", "Memory", "Memory_Speed","L2_Cache","Memory_Bus","Dedicated","Shader","Manufacturer")]
head(main_GPU_data, 10)

# Xác định các biến số và tạo một khung dữ liệu chỉ chứa các biến số
new_numeric_vars <- sapply(main_GPU_data, is.numeric)
numeric_data <- main_GPU_data[, new_numeric_vars]
# Tính toán các thống kê mô tả cho các biến số
summary_stats <- sapply(numeric_data, function(x) {
  c(
    Mean = mean(x),                   # Trung bình
    SD = sd(x),                       # Độ lệch chuẩn
    Min = min(x),                     # Giá trị nhỏ nhất
    Q1 = quantile(x, 0.25),           # Phân vị 25%
    Median = median(x),               # Trung vị
    Q3 = quantile(x, 0.75),           # Phân vị 75%
    Max = max(x)                      # Giá trị lớn nhất
  )
})

# Chuyển đổi cột 'Dedicated' sang kiểu số
main_GPU_data$Dedicated <- as.factor(main_GPU_data$Dedicated)
# Chuyển đổi cột 'Shader' sang kiểu số
main_GPU_data$Shader <- as.factor(main_GPU_data$Shader)
# Chuyển đổi cột 'Manufacturer' sang kiểu số
main_GPU_data$Manufacturer <-as.factor(main_GPU_data$Manufacturer)

# Thống kê số lượng biến Dedicated
summary(main_GPU_data$Dedicated)
# Thống kê số lượng biến Shader
summary(main_GPU_data$Shader)
# Thống kê số lượng biến Manufacturer
summary(main_GPU_data$Manufacturer)

#Thông kê mô tả tổng quát cho các biến Định lượng lẫn Phân loạiloại
summary(main_GPU_data)


# Vẽ histogram cho biến Memory_Bandwidth
ggplot(main_GPU_data, aes(x = Memory_Bandwidth)) +
  geom_histogram(fill = "orchid", color = "black",alpha = 0.7) +  # Thay đổi binwidth theo yêu cầu
  labs(title = "Phân Phối Memory Bandwidth",
       x = "Memory Bandwidth (GB/s)",  # Ghi chú đơn vị nếu cần
       y = "Số Lượng") +
  theme_minimal()

# Chuân hóa Biến Memory_Bandwidth(Y) và các biến ảnh hướng đến Y như “Memory_Speed”,“L2_Cache”,“Memory_Bus”,“Memory”:
main_GPU_data$Memory_Bandwidth <- log(main_GPU_data$Memory_Bandwidth)
main_GPU_data$Memory_Speed <- log(main_GPU_data$Memory_Speed + 1 )
main_GPU_data$L2_Cache <- log(main_GPU_data$L2_Cache + 1 )
main_GPU_data$Memory_Bus <- log(main_GPU_data$Memory_Bus + 1 )
main_GPU_data$Memory <- log(main_GPU_data$Memory +1 )

# Vẽ histogram cho biến Memory_Bandwidth sau khi đã Chuẩn hóa 
ggplot(main_GPU_data, aes(x = Memory_Bandwidth)) +
  geom_histogram(fill = "orchid", color = "black",alpha = 0.7) +  # Thay đổi binwidth theo yêu cầu
  labs(title = "Phân Phối Memory Bandwidth Đã Chuẩn Hóa ",
       x = "Memory Bandwidth (GB/s)",  # Ghi chú đơn vị nếu cần
       y = "Số Lượng") +
  theme_minimal()

# Vẽ scatter plot cho Memory_Bandwidth theo Memory_Speed
ggplot(main_GPU_data, aes(x = Memory_Speed, y = Memory_Bandwidth)) +
  geom_point(color = "forestgreen", alpha = 0.7) +  # Thay đổi màu sắc và độ trong suốt của điểm
  labs(title = "Scatter Plot của Memory Bandwidth theo Memory Speed",
       x = "Memory Speed (MHz)",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal()

# Vẽ scatter plot cho Memory_Bandwidth theo L2_Cache
ggplot(main_GPU_data, aes(x = L2_Cache, y = Memory_Bandwidth)) +
  geom_point(color = "forestgreen", alpha = 0.7) +  # Thay đổi màu sắc và độ trong suốt của điểm
  labs(title = "Scatter Plot của Memory Bandwidth theo L2 Cache",
       x = "L2 Cache (KB)",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal()

# Vẽ scatter plot cho Memory_Bandwidth theo Memory_Bus
ggplot(main_GPU_data, aes(x = Memory_Bus, y = Memory_Bandwidth)) +
  geom_point(color = "forestgreen", alpha = 0.7) +  # Thay đổi màu sắc và độ trong suốt của điểm
  labs(title = "Scatter Plot của Memory Bandwidth theo Memory Bus",
       x = "Memory Bus (bits)",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal()


# Vẽ scatter plot cho Memory_Bandwidth theo Memory 
ggplot(main_GPU_data, aes(x = Memory, y = Memory_Bandwidth)) +
  geom_point(color = "forestgreen", alpha = 0.7) +  # Thay đổi màu sắc và độ trong suốt của điểm
  labs(title = "Scatter Plot của Memory Bandwidth theo Memory ",
       x = "Memory  (MB)",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal()

# Vẽ boxplot cho Memory_Bandwidth theo Dedicated
ggplot(main_GPU_data, aes(x = Dedicated, y = Memory_Bandwidth)) +
  geom_boxplot(fill = "darkslategray", color = "black", alpha = 0.7) +  # Thay đổi màu sắc và độ trong suốt của hộp
  labs(title = "Boxplot của Memory Bandwidth theo Dedicated GPU",
       x = "Dedicated GPU",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  # Điều chỉnh vị trí nhãn trục x


# Vẽ boxplot cho Memory_Bandwidth theo Shader
ggplot(main_GPU_data, aes(x = Shader, y = Memory_Bandwidth)) +
  geom_boxplot(fill = "darkslategray", alpha = 0.7) +  # Thay đổi màu sắc và độ trong suốt
  labs(title = "Boxplot của Memory Bandwidth theo Shader",
       x = "Shader",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Xoay nhãn trục x

# Vẽ boxplot cho Memory_Bandwidth theo Manufacturer
ggplot(main_GPU_data, aes(x = Manufacturer, y = Memory_Bandwidth)) +
  geom_boxplot(fill = "darkslategray", color = "black", alpha = 0.7) +  # Thay đổi màu sắc và độ trong suốt của hộp
  labs(title = "Boxplot của Memory Bandwidth theo Nhà sản xuất (Manufacturer)",
       x = "Nhà sản xuất (Manufacturer)",
       y = "Memory Bandwidth (GB/s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Xoay nhãn trục x để dễ đọc hơn     


#Bài Toán Ước lượng Trung bình 1 mẫu 
#Kiểm tra điều kiện xem biến có phân phối chuẩn 
hist(main_GPU_data$Memory_Bandwidth)

qqnorm(main_GPU_data$Memory_Bandwidth)
qqline(main_GPU_data$Memory_Bandwidth)

#Tính các đặc trưng mẫu cửa biến 
n <- length(main_GPU_data$Memory_Bandwidth) 
X_tb <- mean(main_GPU_data$Memory_Bandwidth)
s <- sd(main_GPU_data$Memory_Bandwidth)
z_critical <- qnorm(p= .05/2, lower.tail = FALSE )
Esilon <-  qnorm(p= .05/2, lower.tail = FALSE ) * s/sqrt(n)

Left_CI <- X_tb - Esilon
Right_CT <- X_tb + Esilon  
data.frame(n, X_tb, s, z_critical, Esilon, Left_CI, Right_CT)

#Bài toán 2 mẫu
#Phân nhóm dữ liệu biến Manufacturer
Group_1_Data <- subset(New_GPU_Data, Manufacturer == "Nvidia")
Group_2_Data <- subset(New_GPU_Data, Manufacturer == "AMD")

#Kiểm tra kiện phân phối chuẩn của biến Memory_Speed thuộc Nhóm 1(Nvidia)
qqnorm(Group_1_Data$Memory_Speed)
qqline(Group_1_Data$Memory_Speed)
shapiro.test(Group_1_Data$Memory_Speed)

#Kiểm tra kiện phân phối chuẩn của biến Memory_Speed thuộc Nhóm 2(AMD).
qqnorm(Group_2_Data$Memory_Speed)
qqline(Group_2_Data$Memory_Speed)
shapiro.test(Group_2_Data$Memory_Speed)

#Tính các đặc trưng mẫu:
n1 <- length(Group_1_Data$Memory_Speed) 
X1_tb <- mean(Group_1_Data$Memory_Speed)
s1 <- sd(Group_1_Data$Memory_Speed)

n2 <- length(Group_2_Data$Memory_Speed) 
X2_tb <- mean(Group_2_Data$Memory_Speed)
s2 <- sd(Group_2_Data$Memory_Speed)

data.frame(n1, X1_tb, s1, n2, X2_tb, s2)

#Tính giá trị kiểm định Z thực nghiệm (Z0):
z0 <- (X1_tb - X2_tb)/sqrt(s1^2/n1 + s2^2/n2)
print(z0)
#Tìm giá trị tới hạn (Z_a)
qnorm(p= .05, lower.tail = FALSE ) #Z(a) với mức ý nghĩ 5% 

#Xét ngược ĐK
var.test(Group_1_Data$Memory_Speed, Group_2_Data$Memory_Speed, alternative = "greater")

t_0 <- (X1_tb - X2_tb)/sqrt(s1^2/n1 + s2^2/n2)
print(t_0)

v <- (((s1^2/n1)+(s2^2/n2))^2)/((((s1^2/n1)^2)/(n1 - 1)) + (((s2^2/n2)^2)/(n2 - 1)))
print(v)


t_A <- qt(p = .05, df = v, lower.tail=FALSE)  
print(t_A)

