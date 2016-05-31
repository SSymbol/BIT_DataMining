数据缺失处理：
withoutNAs.csv：是剔除了含有NA的数据后的结果。
output1:是对withoutNAs数据，通过centralImputation()用最高频率值来填补缺失值后的结果。
output2:对原始数据先删除含有多个NA的行数，剩余198行，再使用线性模型来填充PO4与oPO4的数据
output3:通过数据对象之间的相似性来填补缺失值,通过knnImputation()来实现