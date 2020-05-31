(ns clojure.ypandya614929.salesorder
    (:gen-class))

; Variable declarations
(def customers_dict (atom()))
(def product_dict (atom()))
(def sales_dict (atom()))

; Defining functions
; ------------------
; This function is used to load customer data
(defn loadCustomerData []
	(def split_new_line (clojure.string/split (slurp "cust.txt") #"\n"))
	(def list_data (map (fn[split_bar] (clojure.string/split split_bar #"\|")) split_new_line))
	(doseq [data list_data]
			(def cutomer_data (hash-map :c_id (nth data 0) :name (nth data 1) :add (nth data 2) :phone (nth data 3)))
			(def cutomer_data_obj (hash-map :c_id (nth data 0) :val cutomer_data))
			(swap! customers_dict conj cutomer_data_obj)))

; This function is used to display customer data
(defn printCustomerData []
			(def sorted_customer_dict (sort-by :c_id (deref customers_dict)))
  	(println "=============== Customer Data ===============\n")
   (doseq [data sorted_customer_dict]
   				(def val_obj (get data :val))
       (println (get data :c_id) ":" (get val_obj :name)":"(get val_obj :add)":"(get val_obj :phone)))
  	(println "\n=============================================\n"))


; This function is used to load product data
(defn loadProductData []
	(def split_new_line (clojure.string/split (slurp "prod.txt") #"\n"))
	(def list_data (map (fn[split_bar] (clojure.string/split split_bar #"\|")) split_new_line))
	(doseq [data list_data]
			(def product_data (hash-map :p_id (nth data 0) :name (nth data 1) :price (nth data 2)))
			(def product_data_obj (hash-map :p_id (nth data 0) :val product_data))
			(swap! product_dict conj product_data_obj)))


; This function is used to display product data
(defn printProductData []
			(def sorted_product_dict (sort-by :p_id (deref product_dict)))
  	(println "=============== Product Data ===============\n")
   (doseq [data sorted_product_dict]
   				(def val_obj (get data :val))
       (println (get data :p_id) ":" (get val_obj :name)":"(get val_obj :price)))
  	(println "\n=============================================\n"))


; This function is used to load sales data
(defn loadSalesData []
	(def split_new_line (clojure.string/split (slurp "sales.txt") #"\n"))
	(def list_data (map (fn[split_bar] (clojure.string/split split_bar #"\|")) split_new_line))
	(doseq [data list_data]
			(def sales_data (hash-map :s_id (nth data 0) :c_id (nth data 1) :p_id (nth data 2) :item_count (nth data 3)))
			(def sales_data_obj (hash-map :s_id (nth data 0) :val sales_data))
			(swap! sales_dict conj sales_data_obj)))


; This function is used to display sales data
(defn printSalesData []
			(def sorted_sales_dict (sort-by :s_id (deref sales_dict)))
  	(println "=============== Sales Data ===============\n")
  	(def customers_data (deref customers_dict))
  	(def product_data (deref product_dict))
   (doseq [data sorted_sales_dict]
   				(def val_obj (get data :val))
   				(def c_id (get val_obj :c_id))
   				(def p_id (get val_obj :p_id))
   				(doseq [c_data customers_data]
   							(def c_val_obj (get c_data :val))
										(if (= c_id (get c_val_obj :c_id))
														(def c_name (get c_val_obj :name))
										)	
							)
							(doseq [p_data product_data]
   							(def p_val_obj (get p_data :val))
										(if (= p_id (get p_val_obj :p_id))
														(def p_name (get p_val_obj :name))
										)	
							)
       (println (get data :s_id) ":" c_name":"p_name":"(get val_obj :item_count)))
  	(println "\n=============================================\n"))
	
	
; This function is used to calcuate customer sales data and display the same
(defn printCustomerSalesData [c_name]
			(def customers_data (deref customers_dict))
			(def product_data (deref product_dict))
			(def sales_data (deref sales_dict))
			(def is_name_exist false)
			(doseq [c_data customers_data]
   							(def c_val_obj (get c_data :val))
										(if (= c_name (get c_val_obj :name))
														(do
																(def is_name_exist true)
																(def customer_obj c_val_obj))
										)	
							)
			(if (= is_name_exist false)
							(println "=============== Customer not found ===============\n")
			)
			(if (= is_name_exist true)
						(do
										(def customer_products (atom()))
										(def add_total_price (atom()))
										(doseq [s_data sales_data]
			   							(def s_data_obj (get s_data :val))
													(if (= (get customer_obj :c_id) (get s_data_obj :c_id))
																	(do 
																				(swap! customer_products conj (hash-map :p_id (get s_data_obj :p_id) :qty (get s_data_obj :item_count)))
																	)
													))
										(doseq [p_data product_data]
			   							(def p_data_obj (get p_data :val))
			   											(doseq [c_p_data (deref customer_products)]
			   														(if (= (get c_p_data :p_id) (get p_data_obj :p_id))
			   														(do 
			   																	(def temp_price (* (Double/parseDouble (clojure.string/trim (get p_data_obj :price))) (Integer/parseInt (clojure.string/trim (get c_p_data :qty)))))
			   																	(swap! add_total_price conj temp_price)
			   														))))

										(if (empty? @add_total_price)
														(println (str c_name":")(str "$0")"\n")
      								(println (str c_name":")(str "$" (format "%.2f" (reduce + @add_total_price))) "\n"))
      )))


; This function is used to calcuate product sales count/data and display the same
(defn printProductSalesData [p_name]
			(def product_data (deref product_dict))
			(def sales_data (deref sales_dict))
			(def is_name_exist false)
			(doseq [p_data product_data]
   							(def p_val_obj (get p_data :val))
										(if (= p_name (get p_val_obj :name))
														(do
																(def is_name_exist true)
																(def product_obj p_val_obj))
										)	
							)
			(if (= is_name_exist false)
							(println "=============== Product not found ===============\n")
			)
			(if (= is_name_exist true)
						(do
										(def product_sales (atom()))
										(def add_total_product (atom()))
										(doseq [s_data sales_data]
			   							(def s_data_obj (get s_data :val))
													(if (= (get product_obj :p_id) (get s_data_obj :p_id))
																	(do 
																				(swap! product_sales conj (hash-map :p_id (get s_data_obj :p_id) :qty (get s_data_obj :item_count)))
																	)
													))
										(doseq [p_s_data (deref product_sales)]
													(do 
																(def temp_count (Integer/parseInt (clojure.string/trim (get p_s_data :qty))))
																(swap! add_total_product conj temp_count)
													))

										(if (empty? @add_total_product)
														(println (str p_name":")(str "0")"\n")
      								(println (str p_name":")(reduce + @add_total_product)"\n"))
      )))


; This function is used to display menu and user input
(defn inputList []
(println "*** Sales Menu ***")
(println "------------------")
(println "")
(println "1. Display Customer Table")
(println "2. Display Product Table")
(println "3. Display Sales Table")
(println "4. Total Sales for Customer")
(println "5. Total Count for Product")
(println "6. Exit")
(println "")
(println "Enter an option?")

(def input_val (read-line))

(if (= input_val "1")
      (do
         	(printCustomerData)
         	(inputList)))
(if (= input_val "2")
      (do
         	(printProductData)
         	(inputList)))
(if (= input_val "3")
      (do 
      				(printSalesData)
         	(inputList)))
(if (= input_val "4")
      (do 
          (println "Enter customer name : ")
										(def input_c_name (read-line))
										(printCustomerSalesData input_c_name)
										(inputList)))
(if (= input_val "5")
      (do 
          (println "Enter product name : ")
										(def input_p_name (read-line))
										(printProductSalesData input_p_name)
										(inputList)))
(if (= input_val "6")
      (do (println "=============== Good Bye ===============\n")
          (System/exit 0)))
(println "=============== Select valid option ===============\n")
(inputList))


; Invoking functions
; ------------------
(loadCustomerData)
(loadProductData)
(loadSalesData)
(inputList)