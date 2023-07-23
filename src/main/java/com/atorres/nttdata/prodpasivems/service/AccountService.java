package com.atorres.nttdata.prodpasivems.service;

import com.atorres.nttdata.prodpasivems.client.FeignApiClient;
import com.atorres.nttdata.prodpasivems.client.FeignApiProdActive;
import com.atorres.nttdata.prodpasivems.exception.CustomException;
import com.atorres.nttdata.prodpasivems.model.RequestAccount;
import com.atorres.nttdata.prodpasivems.model.RequestUpdateAccount;
import com.atorres.nttdata.prodpasivems.model.clientms.ClientDto;
import com.atorres.nttdata.prodpasivems.model.creditms.CreditDto;
import com.atorres.nttdata.prodpasivems.model.dao.AccountDao;
import com.atorres.nttdata.prodpasivems.model.dto.AccountDto;
import com.atorres.nttdata.prodpasivems.repository.AccountRepository;
import com.atorres.nttdata.prodpasivems.repository.accountstrategy.AccountStrategy;
import com.atorres.nttdata.prodpasivems.repository.accountstrategy.AccountStrategyFactory;
import com.atorres.nttdata.prodpasivems.utils.RequestMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Service
@Slf4j
public class AccountService {
  /**
   *Repositorio cuentas
   */
  @Autowired
  private AccountRepository accountRepository;
	/**
	 * Cliente conecta cliente-ms
	 */
	@Autowired
	private FeignApiClient feignApiClient;
	/**
	 * Cliente conecta prod-active-ms
	 */
	@Autowired
	private FeignApiProdActive feignApiProdActive;
	/**
	 * Mapper de cuentas
	 */
  @Autowired
  private RequestMapper requestMapper;
	/**
	 * Clase que divide las reglas para las cuentas segun la cliente
	 */
  @Autowired
  private AccountStrategyFactory accountStrategyFactory;

	/**
	 * Metodo que trae una cuenta por su id
	 * @param productId id producto
	 * @return cuenta
	 */
  public Mono<AccountDto> getAccount(String productId) {
    return accountRepository.findAll()
            .filter(ac -> ac.getId().equals(productId))
            .switchIfEmpty(Mono.error(new CustomException(HttpStatus.NOT_FOUND, "No existe la cuenta")))
            .single()
            .flatMap(ac -> accountRepository.findById(productId))
						.map(requestMapper::accountToDto);
  }

  /**
   * Funcion que crear una cuenta segun el id del cliente y el requestaccount
   * @param clientId       id del cliente
   * @param requestAccount request con los datos de la cuenta
   * @return response la relacion client-product
   */
  public Mono<AccountDto> createAccount(String clientId, RequestAccount requestAccount) {
    //obtenemos el cliente
    return this.checkDebts(clientId)
            .switchIfEmpty(Mono.error(new CustomException(HttpStatus.NOT_FOUND, "El cliente no existe")))
            .flatMap(clientdto -> {
              //obtenemos todas las cuentas agregando la nueva
							AccountDto ac = requestMapper.accountToDto(requestAccount,clientId);
              Flux<AccountDto> accountAll = this.getAllAccountsByClient(clientId).concatWith(Flux.just(ac));
              //seleccionamos la estrategia para el tipo de cliente
              AccountStrategy strategy = accountStrategyFactory.getStrategy(clientdto.getTypeClient());
              return strategy.verifyClient(accountAll, Mono.just(requestAccount.getAccountCategory()), this.getAllCredit(clientId));
            })
						.flatMap(exist -> Boolean.FALSE.equals(exist)
										? Mono.error(new CustomException(HttpStatus.BAD_REQUEST, "La cuenta no cumplen los requisitos"))
										: accountRepository.save(requestMapper.accountToDao(requestAccount)))
						.map(requestMapper::accountToDto);
  }

  /**
   * Metodo para obtener todas las cuentas de un cliente
   * @param clientId id de cliente
   * @return devuelve una lista de cuentas
   */
  public Flux<AccountDto> getAllAccountsByClient(String clientId) {
    return accountRepository.findAll()
						.filter(ac -> ac.getClient().equals(clientId))
						.map(requestMapper::accountToDto)
						.switchIfEmpty(Flux.empty());
  }

  /**
   * Funcion que elimina una cuenta segun el clientproduct que pasemos
   * @param accountId id de la cuenta
   * @return retorna un void
   */
  public Mono<Void> delete(String accountId) {
    return accountRepository.findById(accountId)
						.switchIfEmpty(Mono.defer(() -> Mono.error(new CustomException(HttpStatus.NOT_FOUND, "No existe la cuenta"))))
						.flatMap(account -> accountRepository.deleteById(accountId)
										.doOnSuccess(v -> log.info("Cuenta eliminada con exito")));
  }

	/**
	 * Metodo que actualiza una cuenta
	 * @param request request
	 * @return cuenta
	 */
  public Mono<AccountDao> update(RequestUpdateAccount request) {
    return accountRepository.findById(request.getAccountId())
            .switchIfEmpty(Mono.error(new CustomException(HttpStatus.NOT_FOUND, "No la cuenta")))
						.flatMap(account -> {
							//Actualizando balance
							account.setBalance(request.getBalance());
							return accountRepository.save(account);
						});
  }

	/**
	 * Metodo que trae todos los creditos para validaciones de cuentas vip y mype
	 * @param clientId id cliente
	 * @return cuentas
	 */
  private Flux<CreditDto> getAllCredit(String clientId) {
    return feignApiProdActive.getAllCreditClient(clientId);
  }

	/**
	 * Metodo que verifica que el cliente no tenga deudas vencidas y retorna un client
	 * @param clientId client id
	 * @return client
	 */
	private Mono<ClientDto> checkDebts(String clientId){
		return feignApiProdActive.getDeuda(clientId)
						.single()
						.flatMap(value -> {
							if(Boolean.TRUE.equals(value))
								return Mono.error(new CustomException(HttpStatus.CONFLICT, "Cliente tiene deudas vencidas"));
							else
								return feignApiClient.getClient(clientId).single();
						});
	}
}
