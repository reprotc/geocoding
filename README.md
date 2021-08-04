# **Geocoding**
*Scripts em R referentes ao paper ["Improving geocoding matching rates of structured addresses in Rio de Janeiro, Brazil"](https://www.scielosp.org/article/csp/2021.v37n7/e00039321/).
 
__________________________________________________________________________________________________________

### **Descrição geral** 
O processo de georreferencimento incluiu as etapas de padronização dos endereços, geocodificação por meio da API do google maps e validação dos resultados.

*Para a execução sequencial dos scripts é necessário:*

- Baixar os arquivos de dados (endereços, dados referência, dicionários etc) no diretório escolhido. 
 Todos os "dicionários" utilizados são referentes ao Rio de Janeiro, mas podem ser adaptados para outros locais e base de dados.
 [Alguns dicionários contêm expressões regulares](https://raw.githubusercontent.com/rstudio/cheatsheets/master/regex.pdf).

- Para os bancos com formatos diferentes do SIM: [indicar os nomes das colunas do banco de dados referentes aos campos do endereço (logradouro, bairro etc)](https://github.com/direcprovisorio/geocoding/blob/00e4e007b41e06209c8e4fc36329198836c61e17/script1_padronizacao.R#L38-L53)


- Para editar a base de dados dos correios - [Diretório Nacional de Endereço - DNE](http://shopping.correios.com.br/wbm/store/script/wbm2400902p01.aspx?cd_company=ErZW8Dm9i54=&cd_department=SsNp3FlaUpM=) - é necessario salvar (em formato csv) os arquivos da pasta 'delimitado' referentes ao local do estudo: em geral, LOG_LOGRADOURO,      LOG_GRANDE USUARIO e LOG BAIRRO). Para facilitar, salve os arquivos com separacao por '@'.

-  Para utilizar a API do google é necessário [cadastrar o projeto e ativar a API key](https://developers.google.com/maps/documentation/geocoding/start). 
Depois, [indicar no script a chave (geocoding API key) do projeto](https://github.com/direcprovisorio/geocoding/blob/f78cc70386aaf658242329340954602071386725/script5_geocoding_googleAPI.R#L31-L33)

__________________________________________________________________________________________________________

### **Descrição dos scripts**

#### Script 1: **Padronização básica dos endereços** 

- Recuperação das informações de bairro que estão como NA/missing no campo do bairro, mas que aparecem nos campos complemento ou nome do logradouro
- Padronização das informações referentes ao bairro utilizando o dicionário de bairros 
- Recuperacao das informações referentes ao número e ao logradouro por meio do campo complemento
- Padronização do logradouro, correção de erros (mais frequentes) no nome do logradouro, conversão de números por extenso e abreviações, 
- Criação do endereço completo (sem DNE)

#### Script 2: **Edição da base de dados dos correios (DNE)**

- Criação do campo número/intervalo e lado (utilizando o LOG_COMPLEMENTO) e do campo para os nomes alternativos de logradouro e bairro
- Construção das listas de nomes, tipos e pares de tipos de logradouros 
- Padronização básica de todos os campos de endereço do DNE.

#### Script 3: **"Dicionário" automático de logradouros** 

- Criação do dicionário/corretor de logradouro, utilizando regras de erros de grafia aplicadas à base de enderecos dos correios
- Aplicação do dicionário corretor nos endereços do banco original e a criação do endereço completo final 

#### Script 4: **Linkage com a base de dados dos correios (DNE)**

- Recuperação das informações de logradouro e bairro dos endereços com número de cep válido (8 dígitos)
- Linkage com a base DNE (para os endereços sem cep válido), utilizando o logradouro como campo de blocagem.

#### Script 5: **Geocodificação por meio da API do google** 

- Geocodificação (geocode-ggmap)
- Extração dos campos de interesse (status da busca, endereco completo, latitute, longitude, tipo (rooftop, range_interpolated, geometric_center, approximated), matching parcial.

#### Script 6: **Validação dos endereços** 

- Separação e padronização dos campos de endereço retornados pelo google.
- Comparação entre os campos (logradouro, número, bairro, cidade) do endereco original e os campos do endereço retornado pelo google.
- Comparação entre as informações de logradouro e bairro obtidas por meio da base dos correios. 


*Atualizações dos scripts serão publicadas diretamente na aba principal. A versão original estará na pasta (versão_paper2021)*
