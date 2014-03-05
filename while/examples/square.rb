n = 200
@c = 1
@suma = 0

def dodaj_kwadrat
	tmp = 1
	while tmp <= @c * @c do
		@suma = @suma + 1
		tmp = tmp + 1
	end
end

while @c <= n do
	dodaj_kwadrat
	@c = @c + 1
end

puts @suma
