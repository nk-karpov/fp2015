integration :: (Double -> Double) -> Double -> Double -> Double
trapezoid:: (Double -> Double) -> Double -> Double -> Double -> Int -> Int -> Double
integration f a b = trapezoid f 0 a b 0 3000
trapezoid f s a b i n = if i == n 
	then s 
	else trapezoid f (s + area) a b (i + 1) n 
		where area = 0.5 * h * (f (a + h * (fromIntegral (i + 1))) + f (a + h * (fromIntegral i)))
			where h =  (b - a) / (fromIntegral n)
