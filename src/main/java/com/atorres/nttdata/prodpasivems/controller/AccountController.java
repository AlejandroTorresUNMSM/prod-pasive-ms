package com.atorres.nttdata.prodpasivems.controller;

import com.atorres.nttdata.prodpasivems.client.ClientApiClient;
import com.atorres.nttdata.prodpasivems.client.FeignApiClient;
import com.atorres.nttdata.prodpasivems.model.RequestAccount;
import com.atorres.nttdata.prodpasivems.model.clientms.ClientDto;
import com.atorres.nttdata.prodpasivems.model.dto.AccountDto;
import com.atorres.nttdata.prodpasivems.service.AccountService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/account")
@Slf4j
public class AccountController {
	@Autowired
	AccountService accountService;
	@Autowired
	private FeignApiClient feignApiClient;

	/**
	 * Metodo para traer la cuenta
	 * @param productId id producto
	 * @return cuenta
	 */
	@GetMapping(value = "/{productId}",produces = MediaType.TEXT_EVENT_STREAM_VALUE)
	public Mono<AccountDto> getAccount(
					@PathVariable String productId){
		return accountService.getAccount(productId)
						.doOnNext(account -> log.info("Cuenta encontrada con exito"));
	}

	/**
	 * Endpoint para obtener todas las cuentas de un cliente
	 * @param id id del cliente
	 * @return devuelve una lista de cuentas
	 */
	@GetMapping(value = "/client/{id}",produces = MediaType.TEXT_EVENT_STREAM_VALUE)
	public Flux<AccountDto> getAllAccountClient(@PathVariable String id){
		return accountService.getAllAccountsByClient(id)
						.doOnNext(account -> log.info("Cuenta encontrada: "+account.getId()));
	}

	/**
	 * Endpoint para crear una cuenta para un cliente por su id y un RequestAccount
	 * @param id id del cliente
	 * @param requestAccount request con los datos de la cuenta
	 * @return retorna la entidad relacion client-product
	 */
	@PostMapping(value = "/client/{id}", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
	public Mono<AccountDto> createAccount(@PathVariable String id, @RequestBody Mono<RequestAccount> requestAccount){
		return requestAccount.flatMap(account -> accountService.createAccount(id,account)
						.doOnSuccess(v -> log.info("Cuenta creada con exito")));
	}
}
